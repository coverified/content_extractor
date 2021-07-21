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
      url: Option[UrlSelection],
      date: Option[String],
      disabled: Option[Boolean]
  )

  def view[UrlSelection](
      urlSelection: SelectionBuilder[Url, UrlSelection]
  ): SelectionBuilder[Entry, SimpleEntryView[UrlSelection]] =
    (Entry.id ~ Entry.name ~ Entry.content ~ Entry.summary ~ Entry.url(
      urlSelection
    ) ~ Entry.date ~ Entry.disabled).mapN {
      (
          id: String,
          name: Option[String],
          content: Option[String],
          summary: Option[String],
          url: Option[UrlSelection],
          date: Option[String],
          disabled: Option[Boolean]
      ) =>
        SimpleEntryView(id, name, content, summary, url, date, disabled)
    }
}
