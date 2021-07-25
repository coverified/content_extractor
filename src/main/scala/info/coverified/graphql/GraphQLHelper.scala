/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.SelectionBuilder
import com.typesafe.scalalogging.LazyLogging
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  ArticleTagCreateInput,
  ArticleTagWhereInput,
  ArticleTagsCreateInput,
  Mutation,
  Query,
  Source,
  SourceWhereInput,
  Url,
  UrlUpdateInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import info.coverified.graphql.schema.SimpleUrl
import org.asynchttpclient.DefaultAsyncHttpClient
import sttp.capabilities.zio.ZioStreams
import sttp.client3.SttpBackend
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.model.Uri
import zio.{Task, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}

class GraphQLHelper(private val apiUri: Uri, private val authSecret: String)
    extends LazyLogging {
  private val runtime = zio.Runtime.default
  private val client = new DefaultAsyncHttpClient()
  private val backend: SttpBackend[Task, ZioStreams] =
    AsyncHttpClientZioBackend.usingClient(
      runtime,
      client
    )

  /**
    * Queries all available sources
    *
    * @return An optional lists of views onto sources
    */
  def queryAllSources: Option[List[Source.SourceView]] =
    queryWithHeader(
      Query.allSources(where = SourceWhereInput(), skip = 0)(Source.view)
    )

  /**
    * Query the given amount of not yet visited urls
    *
    * @param amount The amount of urls to  query
    * @return An option onto a list of new urls
    */
  def queryNewUrls(amount: Int): Option[List[SimpleUrl.SimpleUrlView]] =
    queryWithHeader(ExtractorQuery.newUrls(amount))

  /**
    * Query all not yet visited urls
    *
    * @param sourceId The identifier of the source, the url shall belong to
    * @return An option onto a list of new urls
    */
  def queryNewUrls(sourceId: String): Option[List[SimpleUrl.SimpleUrlView]] =
    queryWithHeader(ExtractorQuery.newUrls(sourceId))

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
    * Query all existing tags
    *
    * @param tags List of tag names, that are of interest
    * @return All existing tags
    */
  def existingTags(tags: Seq[String]): Seq[ArticleTagView] = {
    val contentFilter = tags.map { tag =>
      ArticleTagWhereInput(name_i = Some(tag))
    }
    queryWithHeader(
      Query.allArticleTags(
        where = ArticleTagWhereInput(
          OR = Option.when(contentFilter.nonEmpty)(contentFilter.toList)
        ),
        skip = 0
      )(ArticleTag.view)
    ).getOrElse(List.empty)
  }

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
  def saveArticleTags(tags: List[String]): Option[List[String]] =
    Option
      .when(tags.nonEmpty)(tags)
      .flatMap { apparentTags =>
        /* If the list of tags is non empty, build and send mutations */
        val inputModels = apparentTags.map { tag =>
          Some(
            ArticleTagsCreateInput(
              data = Some(ArticleTagCreateInput(name = Some(tag)))
            )
          )
        }
        val mutation =
          Mutation.createArticleTags(data = Some(inputModels))(ArticleTag.id)
        sendMutationWithHeader(mutation).map { maybeIds =>
          maybeIds.filter(_.nonEmpty).map(_.get)
        }
      }
      .flatMap {
        /* If the list of ids is empty, make it a None */
        maybeIds =>
          Option.when(maybeIds.nonEmpty)(maybeIds)
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
