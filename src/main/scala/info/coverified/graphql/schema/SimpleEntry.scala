/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{Entry, Url}

object SimpleEntry {
  case class SimpleEntryView[UrlSelection](
      id: String,
      name: Option[String],
      content: Option[String],
      summary: Option[String],
      url: Option[UrlSelection]
  )

  def view[UrlSelection](
      urlSelection: SelectionBuilder[Url, UrlSelection]
  ): SelectionBuilder[Entry, SimpleEntryView[UrlSelection]] =
    (Entry.id ~ Entry.name ~ Entry.content ~ Entry.summary ~ Entry.url(
      urlSelection
    )).mapN {
      (
          id: String,
          name: Option[String],
          content: Option[String],
          summary: Option[String],
          url: Option[UrlSelection]
      ) =>
        SimpleEntryView(id, name, content, summary, url)
    }
}
