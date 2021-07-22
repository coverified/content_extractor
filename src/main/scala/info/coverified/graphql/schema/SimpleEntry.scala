/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  Entry,
  Tag,
  TagWhereInput,
  Url
}

object SimpleEntry {
  case class SimpleEntryView[UrlSelection, TagSelection](
      id: String,
      name: Option[String],
      content: Option[String],
      summary: Option[String],
      url: Option[UrlSelection],
      date: Option[String],
      disabled: Option[Boolean],
      tags: Option[List[TagSelection]]
  )

  def view[UrlSelection, TagSelection](
      urlSelection: SelectionBuilder[Url, UrlSelection],
      tagSelection: SelectionBuilder[Tag, TagSelection]
  ): SelectionBuilder[Entry, SimpleEntryView[UrlSelection, TagSelection]] =
    (Entry.id ~ Entry.name ~ Entry.content ~ Entry.summary ~ Entry.url(
      urlSelection
    ) ~ Entry.date ~ Entry.disabled ~ Entry.tags(
      where = TagWhereInput(),
      skip = 0
    )(tagSelection)).mapN {
      (
          id: String,
          name: Option[String],
          content: Option[String],
          summary: Option[String],
          url: Option[UrlSelection],
          date: Option[String],
          disabled: Option[Boolean],
          tags: Option[List[TagSelection]]
      ) =>
        SimpleEntryView(id, name, content, summary, url, date, disabled, tags)
    }
}
