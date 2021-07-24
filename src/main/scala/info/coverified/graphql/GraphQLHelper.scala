/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  Query,
  Source,
  SourceWhereInput
}
import info.coverified.graphql.schema.SimpleUrl
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.model.Uri

class GraphQLHelper(private val apiUri: Uri, private val authSecret: String) {

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
    * Sends the given request with specified auth header to the GraphQL and directly run the equivalent ZIO-Effect
    *
    * @param selectionBuilder Selection builder to apply
    * @tparam R Type of return
    * @return The result of the query
    */
  private def queryWithHeader[R](
      selectionBuilder: SelectionBuilder[RootQuery, R]
  ): R = zio.Runtime.default.unsafeRun {
    Connector
      .sendRequest(
        selectionBuilder
          .toRequest(apiUri)
          .header("x-coverified-internal-auth", authSecret)
      )
      .provideCustomLayer(AsyncHttpClientZioBackend.layer())
  }
}
