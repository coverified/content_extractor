/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryWhereInput,
  Query,
  Url,
  UrlWhereInput
}

import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneId, ZonedDateTime}

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
        lastCrawl = Some("1970-01-01T00:00:00.000Z")
      ),
      skip = 0,
      first = Some(first)
    )(
      SimpleUrl.view
    )

  /**
    * Query a specified amount of urls, that haven't been handled recently
    *
    * @param first              Amount of urls to query
    * @param reAnalysisInterval Delay between two analyses of a page
    * @return An equivalent [[SelectionBuilder]]
    */
  def existingUrls(
      first: Int,
      reAnalysisInterval: Duration
  ): SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    Query.allUrls(
      where = UrlWhereInput(
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
      ),
      skip = 0,
      first = Some(first)
    )(
      SimpleUrl.view
    )

  /**
    * Query entry, that do exist for the given url ids
    *
    * @param urlId  Id of applicable url
    * @return An equivalent [[SelectionBuilder]]
    */
  def existingEntry(urlId: String): SelectionBuilder[RootQuery, Option[
    List[SimpleEntry.SimpleEntryView[String]]
  ]] =
    Query.allEntries(
      where = EntryWhereInput(
        url = Some(UrlWhereInput(id = Some(urlId)))
      ),
      skip = 0
    )(SimpleEntry.view(Url.id))
}