/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql

import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Tag.TagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  EntryWhereInput,
  Query,
  SourceWhereInput,
  Tag,
  Url,
  UrlWhereInput
}
import info.coverified.graphql.schema.{
  CoVerifiedClientSchema,
  SimpleEntry,
  SimpleUrl
}

import java.time.format.DateTimeFormatter
import java.time.{Duration, ZoneId, ZonedDateTime}

object ExtractorQuery {

  val DUMMY_LAST_CRAWL_DATE_TIME: String = "1970-01-01T00:00:00.000Z"

  private val commonFileEndings = List(
    /* Documents */
    ".epub",
    ".pdf",
    ".doc",
    ".docx",
    ".xls",
    ".xlsm",
    ".xlsx",
    ".ppt",
    ".pptx",
    ".odt",
    ".ods",
    ".zip",
    ".ics",
    ".rss",
    ".rtf",
    /* Images */
    ".png",
    ".jpg",
    ".jpeg",
    ".svg",
    ".gif",
    ".bmp",
    ".eps",
    /* Audio / Video */
    ".wav",
    ".mp4",
    ".mp3",
    ".swf",
    /* Other stuff */
    ".srt",
    ".m4r"
  )

  @deprecated("Use equivalent method in GraphQLHelper")
  private def excludeCommonFiles: List[UrlWhereInput] =
    commonFileEndings.map(
      ending => UrlWhereInput(name_not_contains_i = Some(ending))
    )

  /**
    * Query a specified amount of urls, that haven't been handled yet
    *
    * @param first Amount of urls to query
    * @return An equivalent [[SelectionBuilder]]
    */
  @deprecated("Use equivalent method in GraphQLHelper")
  def newUrls(
      first: Int
  ): SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    newUrls(Some(first))

  /**
    * Query all new urls
    *
    * @return A selection build to query all not yet visited urls
    */
  @deprecated("Use equivalent method in GraphQLHelper")
  def newUrls
      : SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    newUrls(None)

  /**
    * Query all new urls for a given source
    *
    * @param sourceId Id of the source, the url shall belong to
    * @return A selection build to query all not yet visited urls, that belong to a source
    */
  @deprecated("Use equivalent method in GraphQLHelper")
  def newUrls(
      sourceId: String
  ): SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    Query.allUrls(
      where = UrlWhereInput(
        lastCrawl = Some(DUMMY_LAST_CRAWL_DATE_TIME),
        AND = Some(
          excludeCommonFiles ++ List(
            UrlWhereInput(name_not_contains_i = Some("/Videos/")),
            UrlWhereInput(source = Some(SourceWhereInput(id = Some(sourceId))))
          )
        )
      ),
      skip = 0
    )(
      SimpleUrl.view
    )

  @deprecated("Use equivalent method in GraphQLHelper")
  private def newUrls(
      maybeFirst: Option[Int]
  ): SelectionBuilder[RootQuery, Option[List[SimpleUrl.SimpleUrlView]]] =
    Query.allUrls(
      where = UrlWhereInput(
        lastCrawl = Some(DUMMY_LAST_CRAWL_DATE_TIME),
        AND = Some(
          excludeCommonFiles ++ List(
            UrlWhereInput(name_not_contains_i = Some("/Videos/"))
          )
        )
      ),
      skip = 0,
      first = maybeFirst
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
        ),
        lastCrawl_gt = Some(DUMMY_LAST_CRAWL_DATE_TIME),
        AND = Some(
          excludeCommonFiles ++ List(
            UrlWhereInput(name_not_contains_i = Some("/Videos/"))
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
    * @param urlId Id of applicable url
    * @return An equivalent [[SelectionBuilder]]
    */
  def existingEntry(urlId: String): SelectionBuilder[RootQuery, Option[
    List[SimpleEntry.SimpleEntryView[String, ArticleTagView]]
  ]] =
    Query.allEntries(
      where = EntryWhereInput(url = Some(UrlWhereInput(id = Some(urlId)))),
      skip = 0
    )(SimpleEntry.view(Url.id, ArticleTag.view))

  /**
    * Query all entries with the given hash code
    *
    * @param contentHash Hash code of the content
    * @return A List of applicable entries
    */
  def countEntriesWithGivenHash(
      contentHash: String
  ): SelectionBuilder[RootQuery, Option[Int]] =
    Query.entriesCount(
      where = EntryWhereInput(
        contentHash = Some(contentHash),
        disabled = Some(false)
      )
    )
}
