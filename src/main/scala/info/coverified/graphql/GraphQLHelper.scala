/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.SelectionBuilder
import com.typesafe.scalalogging.LazyLogging
import info.coverified.graphql.schema.CoVerifiedClientSchema.Tag.TagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  Mutation,
  Query,
  Source,
  SourceWhereInput,
  Tag,
  TagWhereInput,
  UrlUpdateInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import info.coverified.graphql.schema.{
  CoVerifiedClientSchema,
  SimpleEntry,
  SimpleUrl
}
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
  def entriesWithSameHash(contentHash: String): Boolean =
    queryWithHeader(
      ExtractorQuery
        .entriesWithGivenHash(contentHash)
    ).exists(_.nonEmpty)

  /**
    * Query all existing tags
    *
    * @return All existing tags
    */
  def allExistingTags: Seq[TagView[String]] =
    queryWithHeader(
      Query
        .allTags(
          where = TagWhereInput(generated = Some(false)),
          skip = 0
        )(Tag.view(CoVerifiedClientSchema.Language.id))
    ).getOrElse(List.empty)

  /**
    * Save the entry to data base
    *
    * @param entryMutation Mutation that describes the transformation
    * @return Optional view onto the created entry
    */
  def saveEntry(
      entryMutation: SelectionBuilder[RootMutation, Option[
        SimpleEntryView[SimpleUrlView, TagView[String]]
      ]]
  ): Option[SimpleEntryView[SimpleUrlView, TagView[String]]] =
    sendMutationWithHeader(entryMutation)

  def updateUrl(urlId: String) =
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
      )(SimpleUrl.view)
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
            "Error during execution of query '{}'.",
            selectionBuilder,
            failure
          )
          ZIO.succeed(None)
        },
        success =>
          ZIO.succeed {
            success.body match {
              case Left(error) =>
                logger.error("API returned an error.", error)
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
          logger
            .error("Error during execution of mutation '{}'.", mutation, failure)
          ZIO.succeed(None)
        },
        success =>
          ZIO.succeed {
            success.body match {
              case Left(error) =>
                logger.error("API returned an error.", error)
                None
              case Right(result) =>
                result
            }
          }
      )
  }
}
