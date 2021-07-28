/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.SelectionBuilder
import com.typesafe.scalalogging.LazyLogging
import info.coverified.graphql.GraphQLHelper.{
  isNewUrl,
  needsReAnalysis,
  tagFilter
}
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  ArticleTagCreateInput,
  ArticleTagWhereInput,
  ArticleTagsCreateInput,
  EntryWhereInput,
  Mutation,
  Query,
  Source,
  SourceWhereInput,
  Url,
  UrlUpdateInput,
  UrlWhereInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import info.coverified.graphql.schema.{SimpleEntry, SimpleUrl}

import org.asynchttpclient.DefaultAsyncHttpClient
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.model.Uri
import zio.{Task, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneId, ZonedDateTime}

class GraphQLHelper(
    private val apiUri: Uri,
    private val authSecret: String,
    private val batchSize: Int = 100
) extends LazyLogging {
  private val runtime = zio.Runtime.default
  private val client = new DefaultAsyncHttpClient()
  private val backend: SttpBackend[Task, ZioStreams] =
    AsyncHttpClientZioBackend.usingClient(
      runtime,
      client
    )

  /**
    * Queries all available sources in batches.
    *
    * @return An optional lists of views onto sources
    */
  def queryAllSources: Option[List[Source.SourceView]] =
    countAllSources.flatMap { amountOfSources =>
      val results =
        neededBatches(amountOfSources).flatMap(queryAllSources).flatten
      Option.when(results.nonEmpty)(results.toList)
    }

  private def neededBatches(amount: Int): Seq[Int] =
    0 to math.ceil(amount.toDouble / batchSize).toInt

  /**
    * Count all available sources
    *
    * @return Amount of sources - possibly...
    */
  private def countAllSources: Option[Int] =
    queryWithHeader(Query.sourcesCount(where = SourceWhereInput()))

  /**
    * Query the batch given by count.
    *
    * @param count The count of batch to get
    * @return Optional list of view onto sources
    */
  private def queryAllSources(count: Int): Option[List[Source.SourceView]] =
    queryWithHeader(
      Query.allSources(
        where = SourceWhereInput(),
        first = Some(batchSize),
        skip = count * batchSize
      )(Source.view)
    )

  /**
    * Query all not yet visited urls
    *
    * @param sourceId The identifier of the source, the url shall belong to
    * @return An option onto a list of new urls
    */
  def queryNewUrls(sourceId: String): Option[List[SimpleUrl.SimpleUrlView]] =
    countNewUrls(sourceId).map { amountOfUrls =>
      neededBatches(amountOfUrls)
        .flatMap(batchCount => queryNewUrls(sourceId, batchCount))
        .flatten
        .toList
    }

  /**
    * Count the amount of urls for a given source
    *
    * @param sourceId The given source
    * @return Optional amount of available new urls
    */
  private def countNewUrls(sourceId: String): Option[Int] = queryWithHeader {
    Query.urlsCount(where = isNewUrl(sourceId))
  }

  /**
    * Query the batch given by count.
    *
    * @param count The count of batch to get
    * @return Optional list of view onto new urls of a source
    */
  private def queryNewUrls(
      sourceId: String,
      count: Int
  ): Option[List[SimpleUrlView]] = queryWithHeader {
    Query.allUrls(
      where = isNewUrl(sourceId),
      first = Some(batchSize),
      skip = count * batchSize
    )(SimpleUrl.view)
  }

  /**
    * Query all existing urls, that need to be re-analysed, as the re-analysis interval has expired since last visit.
    *
    * @param sourceId           Identifier of the source, the urls shall belong to
    * @param reAnalysisInterval Minimum duration between two visits of a website
    * @return An optional list of existing urls
    */
  def queryExistingUrls(
      sourceId: String,
      reAnalysisInterval: Duration
  ): Option[List[SimpleUrlView]] = {
    /* Define, what is relevant for re-analysis */
    val mostRecentInstant = "\\[UTC]$".r.replaceAllIn(
      DateTimeFormatter.ISO_DATE_TIME.format(
        ZonedDateTime
          .now(ZoneId.of("UTC"))
          .minusHours(reAnalysisInterval.toHours)
      ),
      ""
    )
    val relevant = needsReAnalysis(sourceId, mostRecentInstant)

    /* Query the relevant urls */
    countExistingUrls(sourceId, relevant).map { amountOfUrls =>
      neededBatches(amountOfUrls)
        .flatMap(
          batchCount => queryExistingUrls(batchCount, relevant)
        )
        .flatten
        .toList
    }
  }

  /**
    * Count the amount of urls for a given source
    *
    * @param sourceId The given source
    * @return Optional amount of available new urls
    */
  private def countExistingUrls(
      sourceId: String,
      needsReAnalysis: UrlWhereInput
  ): Option[Int] = queryWithHeader {
    Query.urlsCount(where = needsReAnalysis)
  }

  /**
    * Query the batch given by count.
    *
    * @param count            The count of batch to get
    * @param needsReAnalysis  Condition filter
    * @return Optional list of view onto urls of a source, that need re-analysis
    */
  private def queryExistingUrls(
      count: Int,
      needsReAnalysis: UrlWhereInput
  ): Option[List[SimpleUrlView]] = queryWithHeader {
    Query.allUrls(
      where = needsReAnalysis,
      first = Some(batchSize),
      skip = count * batchSize
    )(SimpleUrl.view)
  }

  /**
    * Query all entries, that be long to the urls with given ids
    *
    * @param urlIds List of url ids to consider
    * @return An optional list of matching entries
    */
  def queryMatchingEntries(
      urlIds: List[String]
  ): Option[List[SimpleEntryView[SimpleUrlView, ArticleTagView]]] = {
    val filter = matchingEntriesFilter(urlIds)
    countMatchingEntries(filter).map { amountOfEntries =>
      neededBatches(amountOfEntries)
        .flatMap(
          batchCount => queryMatchingEntries(batchCount, filter)
        )
        .flatten
        .toList
    }
  }

  /**
    * Set up an entry filter, that selects all entries belonging to the given url ids
    *
    * @param urlIds List of applicable url ids
    * @return Condition to select matching entries
    */
  private def matchingEntriesFilter(urlIds: List[String]): EntryWhereInput = {
    val urlConditions = urlIds.map { urlId =>
      UrlWhereInput(id = Some(urlId))
    }
    EntryWhereInput(OR = Some(urlConditions.map { urlCondition =>
      EntryWhereInput(url = Some(urlCondition))
    }))
  }

  /**
    * Count the amount of matching entries
    *
    * @param filter Filter for matching entries
    * @return Optional amount of available new urls
    */
  private def countMatchingEntries(filter: EntryWhereInput): Option[Int] =
    queryWithHeader {
      Query.entriesCount(where = filter)
    }

  /**
    * Query the batch given by count.
    *
    * @param count  The count of batch to get
    * @param filter Condition filter
    * @return Optional list of view onto urls of a source, that need re-analysis
    */
  private def queryMatchingEntries(
      count: Int,
      filter: EntryWhereInput
  ): Option[List[SimpleEntryView[SimpleUrlView, ArticleTagView]]] =
    queryWithHeader {
      Query.allEntries(
        where = filter,
        first = Some(batchSize),
        skip = count * batchSize
      )(SimpleEntry.view(SimpleUrl.view, ArticleTag.view))
    }

  /**
    * Check, if there is a entry with same content hash already available
    *
    * @param contentHash The content hash to check against
    * @return True, if there is one
    */
  def existsEntryWithSameHash(contentHash: String): Boolean =
    queryWithHeader(
      ExtractorQuery
        .countEntriesWithGivenHash(contentHash)
    ).exists(_ > 0)

  /**
    * Query all existing, matching tags
    *
    * @param tags The tags
    * @return Possibly a result list
    */
  def matchingTags(tags: List[String]): Option[List[ArticleTagView]] = {
    val filter = tagFilter(tags)
    countExistingTags(filter).map { amountOfMatchingTags =>
      neededBatches(amountOfMatchingTags)
        .flatMap(existingTags(filter, _))
        .flatten
        .toList
    }
  }

  /**
    * Count the amount of existing tags
    *
    * @param tagFilter Filter to identify the tags
    * @return Option onto tag count
    */
  private def countExistingTags(tagFilter: ArticleTagWhereInput): Option[Int] =
    queryWithHeader {
      Query.articleTagsCount(where = tagFilter)
    }

  /**
    * Query all existing tags
    *
    * @param tagFilter List of filter statements
    * @param count     Batch count
    * @return All existing tags
    */
  private def existingTags(
      tagFilter: ArticleTagWhereInput,
      count: Int
  ): Option[List[ArticleTagView]] = queryWithHeader(
    Query.allArticleTags(
      where = tagFilter,
      first = Some(batchSize),
      skip = count * batchSize
    )(ArticleTag.view)
  )

  /**
    * Save the entry to data base
    *
    * @param entryMutation Mutation that describes the transformation
    * @return Optional view onto the created entry
    */
  def saveEntry(
      entryMutation: SelectionBuilder[RootMutation, Option[
        SimpleEntryView[SimpleUrlView, ArticleTagView]
      ]]
  ): Option[SimpleEntryView[SimpleUrlView, ArticleTagView]] =
    sendMutationWithHeader(entryMutation)

  /**
    * Builds and sends mutation to save new article tags
    *
    * @param tags Tags to save
    * @return An optional list of matching ids
    */
  def saveArticleTags(tags: List[String]): List[String] =
    tags
      .sliding(batchSize)
      .flatMap { tagBatch =>
        saveArticleTagBatch(tagBatch).getOrElse(List.empty)
      }
      .toList

  /**
    * Saves a batch of mutations for article tags
    *
    * @param tags Tag input
    * @return Optional list of results
    */
  private def saveArticleTagBatch(tags: List[String]): Option[List[String]] = {
    /* If the list of tags is non empty, build and send mutations */
    val inputModels = tags.map { tag =>
      Some(
        ArticleTagsCreateInput(
          data = Some(ArticleTagCreateInput(name = Some(tag)))
        )
      )
    }
    val mutation =
      Mutation.createArticleTags(data = Some(inputModels))(ArticleTag.id)
    sendMutationWithHeader(mutation)
      .map { maybeIds =>
        maybeIds.filter(_.nonEmpty).map(_.get)
      }
      .flatMap {
        /* If the list of ids is empty, make it a None */
        maybeIds =>
          Option.when(maybeIds.nonEmpty)(maybeIds)
      }
  }

  def updateUrl(urlId: String): Option[String] =
    sendMutationWithHeader(
      Mutation.updateUrl(
        urlId,
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
      )(Url.id)
    )

  /**
    * Sends the given request with specified auth header to the GraphQL and directly run the equivalent ZIO-Effect
    *
    * @param selectionBuilder Selection builder to apply
    * @tparam R Type of return
    * @return The result of the query
    */
  private def queryWithHeader[R](
      selectionBuilder: SelectionBuilder[RootQuery, Option[R]]
  ): Option[R] = runtime.unsafeRun {
    selectionBuilder
      .toRequest(apiUri)
      .header("x-coverified-internal-auth", authSecret)
      .send(backend)
      .foldM(
        failure => {
          logger.error(
            "Error during execution of query.\n\tQuery: {}\n\tError: {} - \"{}\"",
            selectionBuilder.toRequest(apiUri).toString,
            failure.getClass.getSimpleName,
            failure.getMessage
          )
          ZIO.succeed(None)
        },
        success =>
          ZIO.succeed {
            success.body match {
              case Left(error) =>
                logger.error(
                  "API returned an error.\n\tQuery: {}\n\tError: {} - \"{}\".",
                  selectionBuilder.toRequest(apiUri).toString,
                  error.getClass.getSimpleName,
                  error.getMessage()
                )
                None
              case Right(result) =>
                result
            }
          }
      )
  }

  /**
    * Sends the given request with specified auth header to the GraphQL and directly run the equivalent ZIO-Effect
    *
    * @param mutation Selection builder to apply
    * @tparam R Type of return
    * @return The result of the mutation
    */
  private def sendMutationWithHeader[R](
      mutation: SelectionBuilder[RootMutation, Option[R]]
  ): Option[R] = runtime.unsafeRun {
    mutation
      .toRequest(apiUri)
      .header("x-coverified-internal-auth", authSecret)
      .send(backend)
      .foldM(
        failure => {
          logger.error(
            "Error during execution of query.\n\tQuery: {}\n\tError: {} - \"{}\".",
            mutation.toRequest(apiUri).toString,
            failure.getClass.getSimpleName,
            failure.getMessage
          )
          ZIO.succeed(None)
        },
        success =>
          ZIO.succeed {
            success.body match {
              case Left(error) =>
                logger.error(
                  "API returned an error.\n\tMutation: {}\n\tError: {} - \"{}\".",
                  mutation.toRequest(apiUri).toString,
                  error.getClass.getSimpleName,
                  error.getMessage()
                )
                None
              case Right(result) =>
                result
            }
          }
      )
  }

  def close(): Unit = {
    backend.close()
    client.close()
  }
}

object GraphQLHelper {
  private val DUMMY_LAST_CRAWL_DATE_TIME: String = "1970-01-01T00:00:00.000Z"

  private val commonFileEndings = List(
    /* Documents */
    ".epub",
    ".pdf",
    ".doc",
    ".docx",
    ".xls",
    ".xlsm",
    ".xlsx",
    ".ppt",
    ".pptx",
    ".odt",
    ".ods",
    ".zip",
    ".ics",
    ".rss",
    ".rtf",
    /* Images */
    ".png",
    ".jpg",
    ".jpeg",
    ".svg",
    ".gif",
    ".bmp",
    ".eps",
    /* Audio / Video */
    ".wav",
    ".mp4",
    ".mp3",
    ".swf",
    /* Other stuff */
    ".srt",
    ".m4r"
  )

  private def excludeCommonFiles: List[UrlWhereInput] =
    commonFileEndings.map(
      ending => UrlWhereInput(name_not_contains_i = Some(ending))
    )

  /**
    * Set up condition for new urls of a given source
    *
    * @param sourceId The given source
    * @return The url restriction model
    */
  private def isNewUrl(sourceId: String): UrlWhereInput = UrlWhereInput(
    lastCrawl = Some(DUMMY_LAST_CRAWL_DATE_TIME),
    AND = Some(
      excludeCommonFiles ++ List(
        UrlWhereInput(source = Some(SourceWhereInput(id = Some(sourceId))))
      )
    )
  )

  /**
    * Set up condition to find
    *
    * @param sourceId           Identifier of the source
    * @param mostRecentInstant  Most recent instant, that is relevant for re-analysis
    * @return
    */
  private def needsReAnalysis(
      sourceId: String,
      mostRecentInstant: String
  ): UrlWhereInput = UrlWhereInput(
    lastCrawl_lte = Some(mostRecentInstant),
    lastCrawl_gt = Some(DUMMY_LAST_CRAWL_DATE_TIME),
    source = Some(SourceWhereInput(id = Some(sourceId))),
    AND = Some(excludeCommonFiles)
  )

  /**
    * Build a filter to search for given tags
    *
    * @param tags The tags to search for
    * @return An equivalent filter
    */
  def tagFilter(tags: List[String]): ArticleTagWhereInput = {
    val singleFilters = tags.map { tag =>
      ArticleTagWhereInput(name_i = Some(tag))
    }
    ArticleTagWhereInput(
      OR = Option.when(singleFilters.nonEmpty)(singleFilters)
    )
  }
}
