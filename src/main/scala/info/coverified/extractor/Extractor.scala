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
  HandleEntryAndUrlEffect,
  NeededInformation,
  buildEntry,
  buildUrlUpdateMutation,
  getProfile4Url,
  updateEntry
}
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  RawEntryInformation,
  UpdateEntryInformation
}
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.config.Config
import info.coverified.extractor.exceptions.{
  ConfigException,
  ExtractionException
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.Connector
import info.coverified.graphql.schema.{SimpleEntry, SimpleUrl}
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryCreateInput,
  EntryUpdateInput,
  Mutation,
  Query,
  TagRelateToManyInput,
  UrlRelateToOneInput,
  UrlUpdateInput,
  UrlWhereInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import sttp.client3.asynchttpclient.zio.SttpClient
import sttp.model.Uri
import zio.{RIO, UIO, ZIO}
import zio.console.Console

import java.io.File
import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneId, ZonedDateTime}
import scala.util.{Failure, Success, Try}

/**
  * Extracting the content from available urls and post the content to data base
  *
  * @version 0.1
  * @since 26.02.21
  */
final case class Extractor private (
    apiUrl: Uri,
    profileDirectoryPath: String,
    reAnalysisInterval: Duration,
    authSecret: String
) extends LazyLogging {

  /**
    * Build the extraction effect. It consists of the following steps:
    * <ol>
    *   <li>Acquire all needed information in parallel</li>
    *   <li>Find applicable config</li>
    *   <li>Issue content extraction</li>
    * </ol>
    *
    * @return
    */
  def buildExtractionEffect(): ZIO[Console with SttpClient, Throwable, Unit] = {
    /* Getting hands on that ZIO stuff - flat mapping by for-comprehension */
    for {
      NeededInformation(hostNameToProfileConfig, urlViews) <- acquireNeededInformation
      _ <- ZIO.collectAllPar(handleUrls(urlViews, hostNameToProfileConfig))
    } yield ()
  }

  /**
    * Get all needed information for content extraction in parallel prior to actual processing
    *
    * @return An effect, that evaluates to [[NeededInformation]]
    */
  private def acquireNeededInformation
      : ZIO[Console with SttpClient, Throwable, NeededInformation] =
    getAllConfigs(profileDirectoryPath).zipWithPar(getAllUrlViews) {
      case (hostnameToConfig, urlViews) =>
        NeededInformation(hostnameToConfig, urlViews)
    }

  /**
    * Acquire all page profiles available in the given directory path.
    *
    * @param cfgDirectoryPath The path, where the files are located
    * @return A map from pages' url to their applicable config
    */
  private def getAllConfigs(
      cfgDirectoryPath: String
  ): UIO[Map[String, ProfileConfig]] = ZIO.effectTotal {
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
    * Asking the Connector for all available urls + additional information within the data source
    *
    * @return An effect, that evaluates to a list of [[SimpleUrlView]]s
    */
  private def getAllUrlViews
      : ZIO[Console with SttpClient, Throwable, List[SimpleUrlView]] = {
    logger.info("Querying all relevant urls")
    Connector
      .sendRequest {
        buildUrlQuery.toRequest(apiUrl)
      }
      .map(_.map(_.flatten).getOrElse(List.empty))
  }

  /**
    * Build up a query to get all urls
    *
    * @return A selection builder with the equivalent query
    */
  private def buildUrlQuery
      : SelectionBuilder[RootQuery, Option[List[Option[SimpleUrlView]]]] =
    Query.allUrls(
      where = Some(
        UrlWhereInput(
          lastCrawl_lte = Some(
            "\\[UTC]$".r.replaceAllIn(
              DateTimeFormatter.ISO_DATE_TIME.format(
                ZonedDateTime
                  .now(ZoneId.of("UTC"))
                  .minusHours(reAnalysisInterval.toHours)
              ),
              ""
            )
          )
        )
      )
    )(
      SimpleUrl.view
    )

  /**
    * Handle all given urls and return a sequence of joined effects to apply
    *
    * @param urls                     Collection of urls to handle
    * @param hostnameToProfileConfig  Mapping from host name to page profile configuration
    * @return A sequence of effects to apply
    */
  private def handleUrls(
      urls: List[SimpleUrlView],
      hostnameToProfileConfig: Map[String, ProfileConfig]
  ): Seq[RIO[Console with SttpClient, Option[Product]]] =
    urls
      .map(handleUrl(_, hostnameToProfileConfig))
      .flatMap {
        case (maybeEntryView, maybeUrlView) =>
          Seq(maybeEntryView, maybeUrlView)
      }
      .flatten

  /**
    * Handle the received url. At first, needed information are extracted and then, parallely, entry as well as updated
    * mutation are sent to API.
    *
    * @param urlView              View onto the url
    * @param hostToProfileConfig  Mapping from host name to it's profile config
    * @return Tuple of [[Option]]s onto an effects, that might be put to API
    */
  private def handleUrl(
      urlView: SimpleUrlView,
      hostToProfileConfig: Map[String, ProfileConfig]
  ): HandleEntryAndUrlEffect = {
    logger.info("Handling url: {}", urlView.name)
    extractInformation(urlView, hostToProfileConfig) match {
      case Success(scrapedInformation: RawEntryInformation) =>
        handleExtractedInformation(scrapedInformation, urlView)
      case Failure(exception) =>
        /* Scraping of site failed -> Don't update anything */
        logger
          .error("Analysis of url '{}' failed.", urlView.name, exception)
        (None, None)
    }
  }

  /**
    * Handle scraped information by checking, if a new entry has to be created, an old one updated or nothing at all has
    * to be done.
    *
    * @param scrapedInformation Information, that are obtained by visiting the site
    * @param urlView            View onto the url, that is concerned
    * @return A tuple of options onto effects, that handle the entry and the url in database
    */
  private def handleExtractedInformation(
      scrapedInformation: RawEntryInformation,
      urlView: SimpleUrlView
  ): HandleEntryAndUrlEffect = {
    val apparentUrlUpdateEffect = Some(updateUrlView(urlView))
    logger.debug("Scraping of rul '{}' succeeded.", urlView.name)

    /* Scraping of site succeeded */
    checkAgainstExisting(scrapedInformation, urlView.entry)
      .map {
        case Left(
            UpdateEntryInformation(id, title, summary, content, date)
            ) =>
          logger.debug(
            "There already is an entry apparent for url '{}'. Update it.",
            urlView.name
          )
          updateEntry(id, title, summary, content, date)
        case Right(CreateEntryInformation(title, summary, content, date)) =>
          logger.debug(
            "There is no entry apparent for url '{}'. Create one.",
            urlView.name
          )
          buildEntry(urlView.id, title, summary, content, date)
      }
      .map(storeMutation) match {
      case apparentEntryEffect @ Some(_) =>
        /* We either need to update or insert a new entry */
        (apparentEntryEffect, apparentUrlUpdateEffect)
      case None =>
        /* Nothing to be stored or updated */
        (None, apparentUrlUpdateEffect)
    }
  }

  /**
    * Based on the received url view, try to find matching profile and acquire and view onto the entry, that it forms
    * together
    *
    * @param urlView              View onto the url
    * @param urlToProfileConfigs  Mapping from url to profile config to use
    * @param browser              The browser to be used for content extraction
    * @return A trial to get a written mutation
    */
  private def extractInformation(
      urlView: SimpleUrlView,
      urlToProfileConfigs: Map[String, ProfileConfig],
      browser: Browser = JsoupBrowser()
  ): Try[RawEntryInformation] =
    urlView match {
      case SimpleUrlView(_, Some(url), Some(sourceId), _, _) =>
        getProfile4Url(url, urlToProfileConfigs).flatMap(
          getEntryInformation(url, sourceId, _, browser)
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
    * @param browser  The browser to be used for content extraction
    * @return A trial to get information for the page
    */
  private def getEntryInformation(
      url: String,
      urlId: String,
      cfg: ProfileConfig,
      browser: Browser = JsoupBrowser()
  ): Try[RawEntryInformation] =
    Analyzer.run(url, urlId, cfg, browser)

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
      maybeApparentEntry: Option[SimpleEntryView[String]]
  ): Option[Either[UpdateEntryInformation, CreateEntryInformation]] =
    maybeApparentEntry match {
      case Some(
          SimpleEntryView(
            id,
            existingTitle,
            existingContent,
            existingSummary,
            _,
            existingDate
          )
          ) =>
        rawInformation match {
          case RawEntryInformation(
              scrapedTitle,
              scrapedSummary,
              scrapedContent,
              scrapedDate
              ) =>
            Option.when(
              existingTitle != scrapedTitle || existingSummary != scrapedSummary || existingContent != scrapedContent || existingDate != scrapedDate
            )(
              Left(
                UpdateEntryInformation(
                  id,
                  scrapedTitle,
                  scrapedSummary,
                  scrapedContent,
                  scrapedDate
                )
              )
            )
        }
      case None => Some(Right(CreateEntryInformation(rawInformation)))
    }

  /**
    * Announce the derived view to API
    *
    * @param mutation Mutation to denote the content
    * @return The equivalent effect
    */
  private def storeMutation(
      mutation: SelectionBuilder[RootMutation, Option[
        SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
      ]]
  ): RIO[Console with SttpClient, Option[
    SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
  ]] =
    Connector.sendRequest(
      mutation
        .toRequest(apiUrl)
        .header("x-coverified-internal-auth", authSecret)
    )

  /**
    * Build a mutation and send it to API in order to update the url entry
    *
    * @param view The original view to update
    * @return An equivalent effect
    */
  private def updateUrlView(
      view: SimpleUrlView
  ): RIO[Console with SttpClient, Option[SimpleUrlView]] = view match {
    case SimpleUrlView(id, url, sourceId, _, _) =>
      val mutation = buildUrlUpdateMutation(id, url, sourceId)
      Connector.sendRequest(
        mutation
          .toRequest(apiUrl)
          .header("x-coverified-internal-auth", authSecret)
      )
  }
}

object Extractor {
  type HandleEntryAndUrlEffect = (
      Option[
        RIO[Console with SttpClient, Option[SimpleEntryView[SimpleUrlView]]]
      ],
      Option[RIO[Console with SttpClient, Option[SimpleUrlView]]]
  )

  def apply(config: Config): Extractor =
    new Extractor(
      config.apiUri,
      config.profileDirectoryPath,
      config.reAnalysisInterval,
      config.authSecret
    )

  /**
    * Data structure to group all data, that can be loaded in parallel prior to actual information extraction
    *
    * @param hostNameToProfileConfig  Mapping a hostname string to applicable [[ProfileConfig]]
    * @param availableUrlViews        List of all available [[SimpleUrlView]]s
    */
  final case class NeededInformation(
      hostNameToProfileConfig: Map[String, ProfileConfig],
      availableUrlViews: List[SimpleUrlView]
  )

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
    * Build entry for extracted page information based on the different page types available.
    *
    * @param urlId    Identifier of url in database
    * @param title    Title of the page
    * @param summary  Summary of the page
    * @param content  Content of the entry
    * @param date     Date of the article
    * @return A mutation to post to data base
    */
  private def buildEntry(
      urlId: String,
      title: Option[String],
      summary: Option[String],
      content: Option[String],
      date: Option[String]
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
  ]] =
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          name = title,
          content = content,
          summary = summary,
          date = date,
          url = Some(
            UrlRelateToOneInput(connect = Some(UrlWhereUniqueInput(id = urlId)))
          )
        )
      )
    )(
      SimpleEntry.view(SimpleUrl.view)
    )

  /**
    * Build a mutation, that updates the entry
    *
    * @param entryId  Identifier of the entry
    * @param title    Title
    * @param summary  Summary
    * @param content  Content
    * @param date     Date of the article
    * @return A mutation to post to data base
    */
  def updateEntry(
      entryId: String,
      title: Option[String],
      summary: Option[String],
      content: Option[String],
      date: Option[String]
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntry.SimpleEntryView[SimpleUrlView]
  ]] =
    Mutation.updateEntry(
      entryId,
      Some(
        EntryUpdateInput(
          name = title,
          summary = summary,
          content = content,
          hasBeenTagged = Some(false),
          date = date,
          tags = Some(TagRelateToManyInput(disconnectAll = Some(true)))
        )
      )
    )(SimpleEntry.view(SimpleUrl.view))

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
