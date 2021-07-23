/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.SelectionBuilder
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.Extractor.{
  buildEntry,
  buildUrlUpdateMutation,
  connectToOrCreateTag,
  scrape,
  updateEntry
}
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  RawEntryInformation,
  UpdateEntryInformation
}
import info.coverified.extractor.analyzer.{Analyzer, EntryInformation}
import info.coverified.extractor.config.Config
import info.coverified.extractor.exceptions.{
  AnalysisException,
  ConfigException,
  ExtractionException
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.Tag.TagView
import info.coverified.graphql.{Connector, ExtractorQuery}
import info.coverified.graphql.schema.{
  CoVerifiedClientSchema,
  SimpleEntry,
  SimpleUrl
}
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryCreateInput,
  EntryUpdateInput,
  Mutation,
  Query,
  Tag,
  TagCreateInput,
  TagRelateToManyInput,
  TagWhereInput,
  TagWhereUniqueInput,
  UrlRelateToOneInput,
  UrlUpdateInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.jsoup.HttpStatusException
import sttp.client3.SttpClientException.ReadException
import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import sttp.model.Uri
import zio.{IO, RIO, URIO, ZIO}
import zio.console.Console

import java.io.File
import java.net.SocketTimeoutException
import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneId, ZonedDateTime}
import scala.collection.immutable
import scala.util.{Either, Failure, Success, Try}

/**
  * Extracting the content from available urls and post the content to data base
  *
  * @version 0.1
  * @since 26.02.21
  */
final case class Extractor private (
    apiUrl: Uri,
    hostNameToProfileConfig: Map[String, ProfileConfig],
    reAnalysisInterval: Duration,
    authSecret: String,
    chunkSize: Int
) extends LazyLogging {

  /**
    * Visit a batch of urls from database, that have not been visited, yet. The batch size is determined by the
    * attribute "chunkSize".
    *
    * @param repeatDelay Delay until the next evaluation starts (only for debugging)
    * @return An effect, that visits all new urls and extracts the content from web page
    */
  def extractNewOnes(
      repeatDelay: Duration
  ): ZIO[Console with SttpClient, Throwable, Boolean] = {
    logger.info("Attempting to visit {} new, not yet visited urls.", chunkSize)
    handleNewUrls(chunkSize).map { visitedUrls =>
      val isLastChunk = visitedUrls < chunkSize
      logger.info(
        "Handled {} new, not yet visited urls. {}",
        visitedUrls,
        if (isLastChunk) "This was the last chunk."
        else
          s"Repeat until all necessary urls have been visited. Delay the next run for $repeatDelay."
      )
      isLastChunk
    }
  }

  /**
    * Visit a batch of urls from database, that have been visited, yet, but need a refresh. The batch size is determined
    * by the attribute "chunkSize".
    *
    * @param repeatDelay Delay until the next evaluation starts (only for debugging)
    * @return An effect, that re-visits all existing urls and extracts the content from web page
    */
  def extractExistingOnes(
      repeatDelay: Duration
  ): ZIO[Console with SttpClient, Throwable, Boolean] = {
    logger.info("Attempting to re-visit {} yet visited urls.", chunkSize)
    handleExistingUrls(chunkSize).map { visitedUrls =>
      val isLastChunk = visitedUrls < chunkSize
      logger.info(
        "Handled {} yet visited urls. {}",
        visitedUrls,
        if (isLastChunk) "This was the last chunk."
        else
          s"Repeat until all necessary urls have been visited. Delay the next run for $repeatDelay."
      )
      isLastChunk
    }
  }

  /**
    * Handle all new urls by querying those urls, that haven't been visited, yet and in parallel scrape the content as
    * well as update the url entry
    *
    * @return An effect, that evaluates to the amount of received new urls
    */
  def handleNewUrls(
      first: Int
  ): ZIO[Console with SttpClient, Throwable, Int] = {
    logger.info("Attempting to handle new urls.")
    for {
      newUrls <- queryNewUrls(first)
      _ <- {
        logger.debug("Treat {} new urls in parallel.", newUrls.size)
        ZIO.collectAllPar(newUrls.map(handleNewUrl))
      }
      noOfReceivedUrls <- IO.apply(newUrls.size)
    } yield noOfReceivedUrls
  }

  /**
    * Query all urls, that have not been visited yet at all.
    *
    * @param first  The amount of urls to query
    * @return An effect to get those urls
    */
  private def queryNewUrls(
      first: Int
  ): URIO[Console with SttpClient, List[SimpleUrlView]] = {
    logger.debug("Query the next {} new urls", first)
    queryUrls(ExtractorQuery.newUrls(first), exception => {
      logger.error("Requesting not yet handled urls failed.", exception)
      List.empty[SimpleUrlView]
    })
  }

  /**
    * Query all urls with specified selection builder
    *
    * FIXME: Test
    * FIXME: Private
    *
    * @param selectionBuilder How to select and build entities
    * @param errorHandling    How to handle errors
    * @return An unfailable effect onto a list of [[SimpleUrlView]]s
    */
  def queryUrls(
      selectionBuilder: SelectionBuilder[RootQuery, Option[
        List[SimpleUrl.SimpleUrlView]
      ]],
      errorHandling: Throwable => List[SimpleUrlView]
  ): URIO[Console with SttpClient, List[SimpleUrlView]] = {
    Connector
      .sendRequest(
        selectionBuilder
          .toRequest(apiUrl)
          .header("x-coverified-internal-auth", authSecret)
      )
      .fold(
        errorHandling, {
          case Some(urlViews) =>
            logger.debug("Received {} urls.", urlViews.size)
            urlViews
          case None =>
            logger.warn(
              "Did receive empty optional on attempt to query not yet handled urls."
            )
            List.empty[SimpleUrlView]
        }
      )
  }

  /**
    * FIXME: Test
    * FIXME: Private
    *
    * @param url
    * @return
    */
  def handleNewUrl(url: SimpleUrlView): URIO[
    Console with SttpClient,
    (
        Option[SimpleUrlView],
        Option[SimpleEntryView[SimpleUrlView, TagView[String]]]
    )
  ] = {
    logger.debug("Handling not yet visited url '{}' ({}).", url.id, url.name)
    updateUrlView(url).zipPar(scrapeAndCreateNewEntry(url))
  }

  /**
    * Build a mutation and send it to API in order to update the url entry
    *
    * @param view The original view to update
    * @return An equivalent effect
    */
  private def updateUrlView(
      view: SimpleUrlView
  ): URIO[Console with SttpClient, Option[SimpleUrlView]] =
    (view match {
      case SimpleUrlView(id, url, sourceId) =>
        logger.debug(
          "Attempt to update the url '{}' ('{}').",
          view.id,
          view.name
        )
        val mutation = buildUrlUpdateMutation(id, url, sourceId)
        Connector.sendRequest(
          mutation
            .toRequest(apiUrl)
            .header("x-coverified-internal-auth", authSecret)
        )
    }).fold(
      exception => {
        logger.error(
          "Updating url entry for url '{}' ({}) failed.",
          view.id,
          view.name,
          exception
        )
        None
      },
      success => {
        logger.debug(
          "Updating url '{}' ('{}') was successful.",
          view.id,
          view.name.getOrElse("")
        )
        success
      }
    )

  /**
    * Scrapes the url and creates a new entry
    *
    * FIXME: Private
    * FIXME: Test
    *
    * @param url The url to scrape
    * @return An effect to store the entry
    */
  def scrapeAndCreateNewEntry(
      url: SimpleUrlView
  ): URIO[Console with SttpClient, Option[
    SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] =
    ZIO
      .fromTry {
        logger.debug("Scraping url '{}' ({}).", url.id, url.name)
        scrape(url, hostNameToProfileConfig)
      }
      .map(CreateEntryInformation(_))
      .flatMap(
        storeNewEntry(
          url.id,
          _
        )
      )
      .fold(
        exception => {
          exception match {
            case timeOut: SocketTimeoutException =>
              logger.error(
                "Time out during browsing new url '{}' ('{}'). Cannot create an entry for that url.",
                url.id,
                url.name,
                timeOut
              )
            case httpStatusException: HttpStatusException =>
              logger.error(
                "Http error {} during browsing of new url '{}' ('{}'). Cannot create an entry for that url.",
                httpStatusException.getStatusCode,
                url.id,
                url.name,
                httpStatusException
              )
            case aex: AnalysisException =>
              logger.error(
                "Analysis exception during browsing of new url '{}' ('{}'). No entry created.",
                url.id,
                url.name,
                aex
              )
            case unknown =>
              logger.error(
                "Unknown error during browsing of new url '{}' ('{}'). Cannot create an entry for that url.",
                url.id,
                url.name,
                unknown
              )
          }
          None
        },
        success => {
          logger.debug(
            "Storing of new entry for url '{}' ('{}') was successful.",
            url.id,
            url.name.getOrElse("")
          )
          success
        }
      )

  /**
    * Puts the freshly generated entry to database
    *
    * FIXME: Test
    * FIXME: Private
    *
    * @param urlId                  Identifier of the url
    * @param createEntryInformation Information received from webpage
    * @return Effect to put new entry to database
    */
  def storeNewEntry(
      urlId: String,
      createEntryInformation: CreateEntryInformation
  ): URIO[Console with SttpClient, Option[
    SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] =
    createEntryInformation match {
      case cei @ CreateEntryInformation(title, summary, content, date, tags) =>
        /* Check, if there isn't yet an entry with the same content */
        val contentHash = cei.contentHash.toString
        queryEntriesWithSameHash(contentHash)
          .map {
            buildEntryConsideringExistingStuff(
              urlId,
              title,
              summary,
              content,
              date,
              tags,
              contentHash,
              _,
              reAnalysisInterval
            )
          }
          .flatMap(storeMutation)
          .fold(
            exception => {
              logger.error(
                s"Putting freshly generated entry to database failed. Entry information: $cei",
                exception
              )
              None
            },
            success => success
          )
    }

  /**
    * Query entries with the same content hash
    *
    * @param contentHash The queried content hash
    * @return Possibly a list of entries with the same hash
    */
  private def queryEntriesWithSameHash(
      contentHash: String
  ): RIO[Console with SttpClient, Option[
    List[SimpleEntryView[String, TagView[String]]]
  ]] =
    Connector
      .sendRequest(
        ExtractorQuery
          .entriesWithGivenHash(contentHash)
          .toRequest(apiUrl)
          .header("x-coverified-internal-auth", authSecret)
      )

  /**
    * If there are no similar entries apparent, store the given information.
    *
    * @param urlId                Id of the entry's url
    * @param title                Title of the new entry
    * @param summary              Summary of the new entry
    * @param content              Content of the new entry
    * @param date                 Date of the new entry
    * @param maybeApparentEntries Optional list of entries with same content
    * @param timeToNextCrawl   Duration, until the next analysis shall take place
    * @return A mutation or throw an exception
    */
  private def buildEntryConsideringExistingStuff(
      urlId: String,
      title: String,
      summary: Option[String],
      content: Option[String],
      date: Option[String],
      maybeTags: Option[List[String]],
      contentHash: String,
      maybeApparentEntries: Option[
        List[SimpleEntryView[String, TagView[String]]]
      ],
      timeToNextCrawl: Duration
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] = {
    /* Check, if the model needs to be disabled */
    val disable = maybeApparentEntries.forall(_.nonEmpty)
    if (disable) {
      logger.warn(
        s"There is / are already entries available with the same content hash code. They belong to the " +
          s"urls with the following ids. Create an entry, but disable it.\n\t${maybeApparentEntries
            .map(_.map(_.url.getOrElse("Cannot get url id")).mkString("\n\t"))
            .getOrElse("Unable to extract url ids.")}"
      )
    }

    /* Figure out, which tags need to be written */
    val maybeConnectToAndCreateTags =
      maybeTags.map(connectToOrCreateTag(_, apiUrl, authSecret))

    buildEntry(
      urlId,
      title,
      summary,
      content,
      date,
      contentHash,
      timeToNextCrawl,
      disable,
      maybeConnectToAndCreateTags
    )
  }

  /**
    * Announce the derived view to API
    *
    * @param mutation Mutation to denote the content
    * @return The equivalent effect
    */
  private def storeMutation(
      mutation: SelectionBuilder[RootMutation, Option[
        SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView, TagView[String]]
      ]]
  ): RIO[Console with SttpClient, Option[
    SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView, TagView[String]]
  ]] =
    Connector.sendRequest(
      mutation
        .toRequest(apiUrl)
        .header("x-coverified-internal-auth", authSecret)
    )

  /**
    * FIXME: Test
    * FIXME: Private
    *
    * @return
    */
  def handleExistingUrls(
      first: Int
  ): ZIO[Console with SttpClient, Throwable, Int] = {
    logger.info("Attempt to handle yet existing urls.")
    for {
      existingUrls <- queryExistingUrls(first, reAnalysisInterval)
      _ <- ZIO.collectAllPar(
        existingUrls.map(
          existingUrl => handleExistingUrl(existingUrl, hostNameToProfileConfig)
        )
      )
      noOfReceivedUrls <- IO.apply(existingUrls.size)
    } yield noOfReceivedUrls
  }

  /**
    * Query all existing urls, that haven't been visited for a while
    *
    * @param first              The amount of urls to get
    * @param reAnalysisInterval The duration, a url should not be revisited
    * @return An effect to get those urls
    */
  private def queryExistingUrls(
      first: Int,
      reAnalysisInterval: Duration
  ): URIO[Console with SttpClient, List[SimpleUrlView]] = queryUrls(
    ExtractorQuery.existingUrls(first, reAnalysisInterval),
    exception => {
      logger.error("Requesting existent urls failed.", exception)
      List.empty[SimpleUrlView]
    }
  )

  /**
    * FIXME: Test
    * FIXME: Private
    *
    * @param url
    * @param hostNameToProfileConfig
    * @return
    */
  def handleExistingUrl(
      url: SimpleUrlView,
      hostNameToProfileConfig: Map[String, ProfileConfig]
  ): ZIO[Console with SttpClient, Throwable, Unit] = {
    logger.debug("Handling url '{}' ({}).", url.id, url.name)
    for {
      (tryRawEntryInformation, maybeEntries) <- IO
        .apply {
          logger.debug(
            "Scraping already visited url '{}' ({}).",
            url.id,
            url.name
          )
          scrape(url, hostNameToProfileConfig)
        }
        .fold(
          exception => {
            exception match {
              case timeOut: SocketTimeoutException =>
                logger.error(
                  "Time out during browsing already known url '{}' ('{}'). Propagate failure.",
                  url.id,
                  url.name,
                  timeOut
                )
              case httpStatusException: HttpStatusException =>
                logger.error(
                  "Http error {} during browsing of already known url '{}' ('{}'). Propagate failure.",
                  httpStatusException.getStatusCode,
                  url.id,
                  url.name,
                  httpStatusException
                )
            }
            Failure(exception)
          },
          identity
        )
        .zipPar {
          logger.debug("Querying entries for url '{}' ({}).", url.id, url.name)
          Connector
            .sendRequest(
              ExtractorQuery
                .existingEntry(url.id)
                .toRequest(apiUrl)
                .header("x-coverified-internal-auth", authSecret)
            )
        }
      _ <- tryRawEntryInformation match {
        case Success(information) =>
          /* Scraping webpage was successful. Check if an entry needs to be generated or updated */
          (handleExistingEntry(
            url.id,
            information,
            maybeEntries.flatMap(_.headOption)
          ) match {
            case Some(effect) => effect
            case None =>
              logger.debug(
                "No update necessary for url '{}' ({}).",
                url.id,
                url.name
              )
              IO.apply((): Unit)
          }).zipPar(updateUrlView(url))
        case Failure(httpError: HttpStatusException)
            if httpError.getStatusCode == 404 =>
          /* Desired web page is not available. Let it untouched. */
          logger.warn(
            s"The desired web page '${url.name}' cannot be found. If available, mark existing entry as disabled."
          )
          attemptToDisable(maybeEntries)
            .getOrElse {
              logger.debug(
                s"There is no existing entry for url '${url.name}', that could be disabled."
              )
              IO.apply((): Unit)
            }
            .zipPar(updateUrlView(url))
        case Failure(exception) =>
          /* Scraping of web page failed. Due to unknown reason. Do not alter the entry. */
          exception match {
            case _: HttpStatusException | _: SocketTimeoutException =>
              logger.debug(
                "Received failure case for a failure, that has already been logged. Don't update anything."
              )
            case _ =>
              logger.error(
                s"Getting content of url ${url.id} (${url.name}) failed due to the following reason. Let the entry untouched",
                exception
              )
          }

          IO.apply((): Unit).zipPar(updateUrlView(url))
      }
    } yield ()
  }

  /**
    * If there is any entry apparent, disable it. If none is available, return empty optional
    *
    * @param maybeEntries Optional list of entries
    * @return An option onto an disabling effect
    */
  private def attemptToDisable(
      maybeEntries: Option[List[SimpleEntryView[_, _]]]
  ): Option[
    RIO[Console with SttpClient, Option[
      SimpleEntryView[SimpleUrlView, TagView[String]]
    ]]
  ] = maybeEntries.flatMap(_.headOption).map { existingEntry =>
    logger.debug(
      s"Mark the entry with id '${existingEntry.id}' as disabled."
    )
    markAsDisabled(existingEntry.id)
  }

  /**
    * Mark the given entry as disabled.
    *
    * @param id Id of entry to disable
    * @return View onto the disabled entry
    */
  private def markAsDisabled(
      id: String
  ): RIO[Console with SttpClient, Option[
    SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] =
    Connector.sendRequest(
      Mutation
        .updateEntry(
          id,
          Some(EntryUpdateInput(disabled = Some(true)))
        )(
          SimpleEntry
            .view(SimpleUrl.view, Tag.view(CoVerifiedClientSchema.Language.id))
        )
        .toRequest(apiUrl)
        .header("x-coverified-internal-auth", authSecret)
    )

  /**
    * FIXME: Test
    * FIXME: Private
    *
    * @param urlId
    * @param scrapedInformation
    * @param maybeApparentEntry
    * @return
    */
  def handleExistingEntry(
      urlId: String,
      scrapedInformation: RawEntryInformation,
      maybeApparentEntry: Option[SimpleEntryView[String, TagView[String]]]
  ): Option[
    URIO[Console with SttpClient, Option[
      SimpleEntryView[SimpleUrlView, TagView[String]]
    ]]
  ] = {
    logger.debug("Handle possibly existing entry for url '{}'.", urlId)
    (checkAgainstExisting(scrapedInformation, maybeApparentEntry) match {
      case Left(maybeUpdateInformation) =>
        maybeUpdateInformation.map {
          case UpdateEntryInformation(
              id,
              title,
              summary,
              content,
              date,
              tags
              ) =>
            logger.debug(
              "There is an entry existent for url '{}' and an update is needed."
            )
            val contentHash = EntryInformation
              .contentHash(
                title,
                summary.getOrElse(""),
                content.getOrElse(""),
                date.getOrElse("")
              )
              .toString
            updateEntry(
              id,
              title,
              summary,
              content,
              date,
              tags,
              maybeApparentEntry.flatMap(_.tags),
              contentHash,
              reAnalysisInterval,
              disabled = false,
              apiUrl,
              authSecret
            )
        }
      case Right(
          cei @ CreateEntryInformation(title, summary, content, date, tags)
          ) =>
        logger.debug(
          "There is no entry apparent for yet visited url '{}'. Attempt to create a new one."
        )
        /* Check, if there isn't yet an entry with the same content. If so, do nothing, if not, build a new selection
         * builder
         * FIXME: Not the best imaginable solution... Direct evaluation should be omitted if possible */
        val contentHash = cei.contentHash.toString
        Some(
          zio.Runtime.default.unsafeRun(
            queryEntriesWithSameHash(contentHash)
              .provideCustomLayer(AsyncHttpClientZioBackend.layer())
              .map {
                buildEntryConsideringExistingStuff(
                  urlId,
                  title,
                  summary,
                  content,
                  date,
                  tags,
                  contentHash,
                  _,
                  reAnalysisInterval
                )
              }
          )
        )
    }).map {
      storeMutation(_).fold(
        exception => {
          exception match {
            case readException: ReadException =>
              logger.error(
                "Reading from GraphQL-API failed during update or creation entry for url '{}' failed.",
                urlId,
                readException
              )
            case unknown =>
              logger.error(
                "Unknown error during update or creation entry for url '{}' failed.",
                urlId,
                unknown
              )
          }
          None
        },
        success => {
          logger
            .debug("Successfully updated or created entry for url '{}'.", urlId)
          success
        }
      )
    }
  }

  /**
    * Determine, if an existing entry needs to be updated or a new one created. If there is one apparent and the content
    * hasn't changed, no new information are handed in
    *
    * @param rawInformation Information scraped from site
    * @param maybeApparentEntry     Possibly existing entry for given url
    * @return An [[Option]] onto either [[UpdateEntryInformation]] or [[CreateEntryInformation]], depending on what needs to be done
    */
  private def checkAgainstExisting(
      rawInformation: RawEntryInformation,
      maybeApparentEntry: Option[SimpleEntryView[String, TagView[String]]]
  ): Either[Option[UpdateEntryInformation], CreateEntryInformation] =
    maybeApparentEntry match {
      case Some(
          SimpleEntryView(
            id,
            maybeExistingTitle,
            existingContent,
            existingSummary,
            _,
            existingDate,
            _,
            maybeExistingTags
          )
          ) =>
        rawInformation match {
          case RawEntryInformation(
              scrapedTitle,
              scrapedSummary,
              scrapedContent,
              scrapedDate,
              maybeTags
              ) =>
            /* Check, if at least one of the different parts of the entry has changed */
            val tagsChanged = tagsHaveChanged(maybeExistingTags, maybeTags)

            Left(
              Option.when(
                !maybeExistingTitle
                  .contains(scrapedTitle) || existingSummary != scrapedSummary || existingContent != scrapedContent || existingDate != scrapedDate || tagsChanged
              )(
                UpdateEntryInformation(
                  id,
                  scrapedTitle,
                  scrapedSummary,
                  scrapedContent,
                  scrapedDate,
                  maybeTags
                )
              )
            )
        }
      case None => Right(CreateEntryInformation(rawInformation))
    }

  /**
    * Figure out, if something regarding the page provided tags has changed
    *
    * @param maybeExistingTags Collection of tags registered in existing entry
    * @param maybePageTags     Tags from web page
    * @return true, if something has changed
    */
  def tagsHaveChanged(
      maybeExistingTags: Option[List[TagView[String]]],
      maybePageTags: Option[List[String]]
  ): Boolean =
    maybeExistingTags
      .map { existingTags =>
        val existingPageTags = existingTags.filter(_.generated.contains(false))
        existingPageTags.size != maybePageTags
          .map(_.size)
          .getOrElse(0) || maybePageTags
          .map(
            tags =>
              !tags.forall(
                tag => existingTags.exists(_.name.contains(tag))
              )
          )
          .getOrElse {
            /* Tags from web page are empty. Something has changed, if there are page tags apparent in the known entry. */
            existingPageTags.nonEmpty
          }
      }
      .getOrElse {
        /* Tags in existing entry are empty. There has something changed, if the page tags aren't empty */
        maybePageTags.nonEmpty && maybePageTags.exists(_.nonEmpty)
      }
}

object Extractor extends LazyLogging {
  type HandleEntryAndUrlEffect = (
      Option[
        RIO[Console with SttpClient, Option[
          SimpleEntryView[SimpleUrlView, TagView[String]]
        ]]
      ],
      Option[RIO[Console with SttpClient, Option[SimpleUrlView]]]
  )

  def apply(
      config: Config,
      hostNameToProfileConfig: Map[String, ProfileConfig]
  ): Extractor =
    new Extractor(
      config.apiUri,
      hostNameToProfileConfig,
      config.reAnalysisInterval,
      config.authSecret,
      config.chunkSize
    )

  /**
    * Acquire all page profiles available in the given directory path.
    *
    * @param cfgDirectoryPath The path, where the files are located
    * @return A map from pages' url to their applicable config
    */
  def getAllConfigs(
      cfgDirectoryPath: String
  ): Map[String, ProfileConfig] = {
    logger.info("Reading in all configs")
    val cfgDirectory = new File(cfgDirectoryPath)
    if (cfgDirectory.exists() && cfgDirectory.isDirectory) {
      cfgDirectory.listFiles
        .filter(_.isFile)
        .map(file => ProfileConfig(ConfigFactory.parseFile(file)))
        .map(profileCfg => profileCfg.profile.hostname -> profileCfg)
        .toMap
    } else {
      Map.empty[String, ProfileConfig]
    }
  }

  /**
    * Given a map of hostnames to configs, find that entry, whose hostname is included within the queried url.
    *
    * @param url              Queried url
    * @param hostNameToConfig Mapping a host name onto it's applicable config
    * @return An [[Option]] onto an applicable config
    */
  private def getProfile4Url(
      url: String,
      hostNameToConfig: Map[String, ProfileConfig]
  ): Try[ProfileConfig] =
    hostNameToConfig
      .find {
        case (hostname, _) =>
          url.contains(hostname)
      }
      .fold[Try[ProfileConfig]] {
        Failure(ConfigException(s"Unable to get config for url '$url'."))
      } { case (_, config) => Success(config) }

  /**
    * Based on the received url view, try to find matching profile and acquire and view onto the entry, that it forms
    * together
    *
    * @param urlView              View onto the url
    * @param urlToProfileConfigs  Mapping from url to profile config to use
    * @return A trial to get a written mutation
    */
  def scrape(
      urlView: SimpleUrlView,
      urlToProfileConfigs: Map[String, ProfileConfig]
  ): Try[RawEntryInformation] =
    urlView match {
      case SimpleUrlView(_, Some(url), Some(sourceId)) =>
        getProfile4Url(url, urlToProfileConfigs)
          .flatMap(
            getEntryInformation(url, sourceId, _)
          )
      case _ =>
        Failure(
          ExtractionException(
            s"Unable to extract information, as at least url or source are not known for url view '$urlView'."
          )
        )
    }

  /**
    * Issue the analyser and try to mutate the given url together with the config into an entry
    *
    * @param url      Queried url
    * @param urlId    The id of the url
    * @param cfg      [[ProfileConfig]] to use
    * @return A trial to get information for the page
    */
  private def getEntryInformation(
      url: String,
      urlId: String,
      cfg: ProfileConfig
  ): Try[RawEntryInformation] =
    Analyzer.run(url, urlId, cfg)

  /**
    * Build entry for extracted page information based on the different page types available.
    *
    * @param urlId              Identifier of url in database
    * @param title              Title of the page
    * @param summary            Summary of the page
    * @param content            Content of the entry
    * @param date               Date of the article
    * @param contentHash        The content hash
    * @param disabled           If the entry is disabled
    * @param timeToNextCrawl Duration, until the next analysis shall take place
    * @return A mutation to post to data base
    */
  private def buildEntry(
      urlId: String,
      title: String,
      summary: Option[String],
      content: Option[String],
      date: Option[String],
      contentHash: String,
      timeToNextCrawl: Duration,
      disabled: Boolean = false,
      maybeConnectToAndCreateTags: Option[
        (Seq[TagWhereUniqueInput], Seq[TagCreateInput])
      ]
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView, TagView[String]]
  ]] =
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          name = Some(title),
          content = content,
          summary = summary,
          date = date,
          url = Some(
            UrlRelateToOneInput(
              connect = Some(UrlWhereUniqueInput(id = Some(urlId)))
            )
          ),
          contentHash = Some(contentHash),
          disabled = Some(disabled),
          nextCrawl = determineNextCrawl(timeToNextCrawl),
          tags = buildTagRelationInput(maybeConnectToAndCreateTags)
        )
      )
    )(
      SimpleEntry
        .view(SimpleUrl.view, Tag.view(CoVerifiedClientSchema.Language.id))
    )

  /**
    * Build correct relation input for tags, taking empty relations into account (e.g. an empty connect to list is
    * represented as None instead)
    *
    * @param maybeConnectToAndCreateTags An optional tuple of relations to build
    * @return An optional model for setting up tag relations properly
    */
  private def buildTagRelationInput(
      maybeConnectToAndCreateTags: Option[
        (
            Seq[CoVerifiedClientSchema.TagWhereUniqueInput],
            Seq[CoVerifiedClientSchema.TagCreateInput]
        )
      ]
  ): Option[TagRelateToManyInput] = maybeConnectToAndCreateTags.flatMap {
    case (connectRelation, createRelation) =>
      val maybeConnectRelation = Option.when(connectRelation.nonEmpty)(
        connectRelation.map(Some(_)).toList
      )
      val maybeCreateRelation =
        Option.when(createRelation.nonEmpty)(createRelation.map(Some(_)).toList)

      (maybeCreateRelation, maybeConnectRelation) match {
        case (None, None) => None
        case (create, connect) =>
          Some(
            TagRelateToManyInput(
              create = create,
              connect = connect
            )
          )
      }
  }

  /**
    * Determine, to which tags to connect to and which need to be created
    *
    * @param tags Selection of tags from web page
    * @param apiUrl     Place, where to reach the GraphQL API
    * @param authSecret Authentication token to use
    * @return A tuple of model to describe connections or creations
    */
  def connectToOrCreateTag(
      tags: Seq[String],
      apiUrl: Uri,
      authSecret: String
  ): (Seq[TagWhereUniqueInput], Seq[TagCreateInput]) = {
    val existingTags = getExistingTags(apiUrl, authSecret)

    /* Build connections to existing entries */
    val tagToMatchingTag: Map[String, Option[TagWhereUniqueInput]] =
      mapTagToExistingTag(tags, existingTags)

    /* Build query to create new tags */
    val createTags = createModelToCreateTag(tagToMatchingTag)

    (tagToMatchingTag.values.flatten.toSeq, createTags)
  }

  /**
    * Query all known tags, that not have been generated by the tagging service
    *
    * @param apiUrl     Place, where to reach the GraphQL API
    * @param authSecret Authentication token to use
    * @return A list of applicable tags
    */
  def getExistingTags(apiUrl: Uri, authSecret: String): Seq[TagView[String]] =
    zio.Runtime.default
      .unsafeRun(
        Connector
          .sendRequest(
            Query
              .allTags(
                where = TagWhereInput(generated = Some(false)),
                skip = 0
              )(Tag.view(CoVerifiedClientSchema.Language.id))
              .toRequest(apiUrl)
              .header("x-coverified-internal-auth", authSecret)
          )
          .provideCustomLayer(AsyncHttpClientZioBackend.layer())
      )
      .getOrElse(List.empty)

  /**
    * Map the web page defined tags against known ones. Return a model to connect to that tag later.
    *
    * @param tags         Collection of web page defined tags
    * @param existingTags Collection of yet existing tags
    * @return A mapping from web page defined tags to an optional model to connect to that tag
    */
  def mapTagToExistingTag(
      tags: Seq[String],
      existingTags: Seq[TagView[String]]
  ): Map[String, Option[TagWhereUniqueInput]] =
    tags.map { tag =>
      tag -> existingTags
        .find { existingTag =>
          existingTag.name match {
            case Some(name) => name == tag
            case None       => false
          }
        }
        .map { matchedTag =>
          TagWhereUniqueInput(id = Some(matchedTag.id))
        }
    }.toMap

  /**
    * Create a collection of models for later creation of tags
    *
    * @param tagToMatchingTag Mapping from web page defined tag to model for tag connection
    * @return A sequence of models to create tags
    */
  def createModelToCreateTag(
      tagToMatchingTag: Map[String, Option[TagWhereUniqueInput]]
  ): Seq[TagCreateInput] =
    tagToMatchingTag
      .filter(_._2.isEmpty)
      .map {
        case (tag, _) =>
          TagCreateInput(name = Some(tag), generated = Some(false))
      }
      .toSeq

  /**
    * Build a mutation, that updates the entry
    *
    * @param entryId      Identifier of the entry
    * @param title        Title
    * @param summary      Summary
    * @param content      Content
    * @param date         Date of the article
    * @param contentHash  The hash of the updated content
    * @param disabled     If the content is disabled
    * @param apiUrl       Place, where to reach the GraphQL API
    * @param authSecret   Authentication token to use
    * @return A mutation to post to data base
    */
  def updateEntry(
      entryId: String,
      title: String,
      summary: Option[String],
      content: Option[String],
      date: Option[String],
      maybePageTags: Option[List[String]],
      maybeExistingTags: Option[List[TagView[String]]],
      contentHash: String,
      timeToNextCrawl: Duration,
      disabled: Boolean = false,
      apiUrl: Uri,
      authSecret: String
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntry.SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] = {
    /* Handle existing entries, take especially care of what has been generated and what has been provided by page.
     * The agreement is, that all generated tags are disconnected. Additionally, all page tags are removed, that are not
     * apparent anymore. */
    val disconnectFrom =
      determineTagsToDisconnectFrom(maybeExistingTags, maybePageTags)
    val maybeConnectAndCreatePageTags =
      maybePageTags.map(connectToOrCreateTag(_, apiUrl, authSecret))

    Mutation.updateEntry(
      entryId,
      Some(
        EntryUpdateInput(
          name = Some(title),
          summary = summary,
          content = content,
          hasBeenTagged = Some(false),
          date = date,
          tags = buildTagRelationInput(maybeConnectAndCreatePageTags),
          contentHash = Some(contentHash),
          disabled = Some(disabled),
          nextCrawl = determineNextCrawl(timeToNextCrawl)
        )
      )
    )(
      SimpleEntry
        .view(SimpleUrl.view, Tag.view(CoVerifiedClientSchema.Language.id))
    )
  }

  /**
    * Determine a list of models, that describe, from which tags to disconnect from. Those are all generated ones and
    * those, that are not yet apparent on the page anymore.
    *
    * @param maybeExistingTags  Optional list of yet know tags for that entry
    * @param maybePageTags      Optional list of tags on the web page
    * @return An optional List of models, that describe tags
    */
  private def determineTagsToDisconnectFrom(
      maybeExistingTags: Option[List[TagView[String]]],
      maybePageTags: Option[List[String]]
  ): Option[List[Some[TagWhereUniqueInput]]] = {
    val disconnectFromGenerated = maybeExistingTags.map { existingTags =>
      existingTags
        .filter(_.generated.contains(true))
        .map(tag => Some(TagWhereUniqueInput(id = Some(tag.id))))
    }
    val disconnectFromRemoved = maybeExistingTags.map(
      existingTags =>
        existingTags
          .filter(isPageTagAndRemoved(_, maybePageTags.getOrElse(List.empty)))
          .map(tag => Some(TagWhereUniqueInput(id = Some(tag.id))))
    )
    val listOfTags = disconnectFromGenerated.getOrElse(List.empty) ++ disconnectFromRemoved
      .getOrElse(List.empty)
    Option.when(listOfTags.nonEmpty)(listOfTags)
  }

  /**
    * Check, if the given existing tag is a page tag and if it has been removed from currently crawled page tags
    */
  private val isPageTagAndRemoved: (TagView[String], List[String]) => Boolean =
    (existingTag: TagView[String], pageTags: List[String]) =>
      existingTag.generated.contains(false) && (existingTag.name match {
        case Some(name) => !pageTags.contains(name)
        case None       => true
      })

  /**
    * Determine the instant of the next crawl
    *
    * @param timeToNextCrawl Duration, when the next crawl happens
    * @return An Option onto a String
    */
  def determineNextCrawl(timeToNextCrawl: Duration): Option[String] = {
    val nextCrawlDateTime =
      ZonedDateTime.now(ZoneId.of("UTC")).plus(timeToNextCrawl)
    Some(
      "\\[UTC]$".r.replaceAllIn(
        DateTimeFormatter.ISO_DATE_TIME.format(nextCrawlDateTime),
        ""
      )
    )
  }

  /**
    * Build the mutation, that is used for updating the url entry in database
    *
    * @param id       Id of entry
    * @param url      Url
    * @param maybeSourceId Id of source
    * @return The needed mutation
    */
  private def buildUrlUpdateMutation(
      id: String,
      url: Option[String],
      maybeSourceId: Option[String]
  ): SelectionBuilder[RootMutation, Option[SimpleUrlView]] =
    Mutation.updateUrl(
      id,
      Some(
        UrlUpdateInput(
          lastCrawl = Some(
            "\\[UTC]$".r.replaceAllIn(
              DateTimeFormatter.ISO_DATE_TIME
                .format(ZonedDateTime.now(ZoneId.of("UTC"))),
              ""
            )
          )
        )
      )
    )(SimpleUrl.view)
}
