/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleSource.SimpleSourceView

object SimpleUrl {

  /**
    * Little sister of [[Url]], but without resolving linked information
    *
    * @param id             Identifier
    * @param name           The actual url
    * @param sourceId       Id of the source to belong to
    * @param entry          Entry, that belongs to the url
    * @param hasBeenCrawled True, if that url has been crawled before
    */
  final case class SimpleUrlView(
      id: String,
      name: Option[String],
      sourceId: Option[String],
      entry: Option[SimpleEntryView[String]],
      hasBeenCrawled: Boolean
  )

  def view: SelectionBuilder[Url, SimpleUrlView] =
    (Url.id ~ Url.name ~ Url.source(SimpleSource.view) ~ Url.lastCrawl)
      .mapN {
        (
            id,
            name,
            source: Option[SimpleSourceView],
            lastCrawl: Option[String]
        ) =>
          SimpleUrlView(
            id,
            name,
            source.map(_.id),
            None,
            lastCrawl.exists(_ != "1970-01-01T00:00:00.000Z")
          )
      }

  def urlId: SelectionBuilder[Url, String] = Url.id
}
