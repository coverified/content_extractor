/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  Query,
  UrlWhereInput
}

object ExtractorQuery {

  /**
    * Query a specified amount of urls, that haven't been handled yet
    *
    * @param first  Amount of urls to query
    * @return An equivalent [[SelectionBuilder]]
    */
  def newUrls(
      first: Int
  ): SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    Query.allUrls(
      where = UrlWhereInput(
        OR = Some(
          List(
            UrlWhereInput(lastCrawl = Some("1970-01-01T00:00:00.000Z")),
            UrlWhereInput(lastCrawl = Some(""))
          )
        )
      ),
      skip = 0,
      first = Some(first)
    )(
      SimpleUrl.view
    )
}
