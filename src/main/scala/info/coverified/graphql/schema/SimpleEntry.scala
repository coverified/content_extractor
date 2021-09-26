/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  ArticleTagWhereInput,
  Entry,
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
      articleTags: Option[List[TagSelection]],
      eTag: Option[String]
  )

  def view[UrlSelection, TagSelection](
      urlSelection: SelectionBuilder[Url, UrlSelection],
      tagSelection: SelectionBuilder[ArticleTag, TagSelection]
  ): SelectionBuilder[Entry, SimpleEntryView[UrlSelection, TagSelection]] =
    (Entry.id ~ Entry.name ~ Entry.content ~ Entry.summary ~ Entry.url(
      urlSelection
    ) ~ Entry.date ~ Entry.disabled ~ Entry.articleTags(
      where = ArticleTagWhereInput(),
      skip = 0
    )(tagSelection) ~ Entry.eTag).mapN {
      (
          id: String,
          name: Option[String],
          content: Option[String],
          summary: Option[String],
          url: Option[UrlSelection],
          date: Option[String],
          disabled: Option[Boolean],
          articleTags: Option[List[TagSelection]],
          eTag: Option[String]
      ) =>
        SimpleEntryView(
          id,
          name,
          content,
          summary,
          url,
          date,
          disabled,
          articleTags,
          eTag
        )
    }
}
