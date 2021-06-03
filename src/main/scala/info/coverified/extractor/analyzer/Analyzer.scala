/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema._
import info.coverified.extractor.profile.ProfileConfig.PageType.Selectors
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.Document

import java.time.ZonedDateTime
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.Extractor
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig.PageType

import scala.util.{Failure, Success, Try}

/**
  * Instance that is able to fetch information from a given url and analyse it's content with respect to a given profile
  * defined for that url
  *
  * @version 0.1
  * @since 26.02.21
  */
object Analyzer extends LazyLogging {

  def run(
      url: String,
      sourceId: String,
      cfg: ProfileConfig,
      browser: Browser = JsoupBrowser()
  ): Try[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    Try(browser.get(url)).flatMap(analyze(url, _, sourceId, cfg))

  /**
    * Analyze the given page document and extract information
    *
    * @param url            Url of page
    * @param pageDoc        Page document
    * @param sourceId       Id of source
    * @param profileConfig  Applicable profile config for this page
    * @return An [[Option]] onto an [[Option]] of a selection builder
    */
  private def analyze(
      url: String,
      pageDoc: Document,
      sourceId: String,
      profileConfig: ProfileConfig
  ): Try[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    determinePageType(url, pageDoc, profileConfig).flatMap {
      case (pageType, selectors) =>
        buildEntry(url, pageDoc, pageType, sourceId, selectors)
    }

  /**
    * Determine the page type (in terms of it's "name") as well as the associated selectors for this type of document
    *
    * @param url            Url of page
    * @param pageDoc        Page document
    * @param profileConfig  Applicable profile configuration
    * @return Option onto a tuple of page type and associated selectors
    */
  private def determinePageType(
      url: String,
      pageDoc: Document,
      profileConfig: ProfileConfig
  ): Try[(String, Selectors)] = {
    profileConfig.profile.pageTypes
      .find(
        pageType =>
          selectorMatches(pageDoc, pageType) && pathMatches(url, pageType)
      )
      .fold[Try[(String, Selectors)]] {
        Failure(
          AnalysisException(s"Unable to gather profile config for url '$url'.")
        )
      } {
        case PageType(_, _, name, selectors) => Success((name, selectors))
      }
  }

  /**
    * Check, if the page document is covered by the given profile config. If no selector is set, let it pass.
    *
    * @param pageDoc  Page document
    * @param pageType Type of the page
    * @return True, if selector matches or no selector is set
    */
  private def selectorMatches(
      pageDoc: Document,
      pageType: ProfileConfig.PageType
  ): Boolean =
    pageType.condition.selector.forall(selector => {
      pageDoc >/~ validator(elementList(selector))(_.nonEmpty) match {
        case Left(_)  => false
        case Right(_) => true
      }
    })

  /**
    * Check, if the configured path is contained within the given url
    *
    * @param url      Url of the page
    * @param pageType Type of the page
    * @return True, the url covers the path or no path is set
    */
  private def pathMatches(
      url: String,
      pageType: ProfileConfig.PageType
  ): Boolean = pageType.condition.path.forall(url.contains(_))

  /**
    * Build entry for extracted page information based on the different page types available.
    *
    * @param url        Url of page
    * @param pageDoc    Page document
    * @param pageType   Type of page
    * @param sourceId   Identifier of the source
    * @param selectors  Selector description to gather information
    * @return
    */
  private def buildEntry(
      url: String,
      pageDoc: Document,
      pageType: String,
      sourceId: String,
      selectors: Selectors
  ): Try[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    (pageType, selectors) match {
      case ("url", selectors) =>
        // build url entry
        Success(buildUrlEntry(pageDoc, url, selectors, sourceId))
      case ("video", selectors) =>
        Success(buildVideoEntry(pageDoc, url, selectors, sourceId))
      case (unknown, _) =>
        Failure(AnalysisException(s"Unknown page type: $unknown"))
    }

  /**
    * Build an entry with extracted page information for a typical url entry
    *
    * @param pageDoc    Page document
    * @param url        Url of the page
    * @param selectors  Applicable selectors
    * @param sourceId   Id of source
    * @return Applicable [[caliban.client.SelectionBuilder]] for the target mutation
    */
  private def buildUrlEntry(
      pageDoc: Document,
      url: String,
      selectors: Selectors,
      sourceId: String
  ): SelectionBuilder[RootMutation, Option[Extractor.EntryView]] =
    extractUrlViewInformation(pageDoc, selectors) match {
      case UrlViewInformation(
          title,
          subTitle,
          summary,
          content,
          publishDate,
          breadCrumbs,
          imageSrc
          ) =>
        createUrlEntry(
          url,
          sourceId,
          title,
          subTitle,
          summary,
          content,
          publishDate,
          breadCrumbs,
          imageSrc
        )
    }

  /**
    * Extract the needed information from page document
    *
    * @param pageDoc    Page document to use
    * @param selectors  Selectors to use
    * @return Gathered information for url page
    */
  private def extractUrlViewInformation(
      pageDoc: Document,
      selectors: Selectors
  ): UrlViewInformation = UrlViewInformation(
    title = pageDoc >?> text(selectors.title),
    subTitle = selectors.subtitle.flatMap(pageDoc >?> text(_)),
    summary = selectors.summary.flatMap(pageDoc >?> text(_)),
    content = pageDoc >?> text(selectors.content),
    publishDate = selectors.date.flatMap(pageDoc >?> text(_)),
    breadCrumbs = selectors.breadcrumb.flatMap(pageDoc >?> text(_)),
    imageSrc = selectors.image.flatMap(pageDoc >?> attr("src")(_))
  )

  /**
    * Actually building the entry with all needed information
    *
    * @param url              Url of page
    * @param sourceId         Source identifier
    * @param maybeTitle       Option onto title
    * @param maybeSubTitle    Option onto subtitle
    * @param maybeSummary     Option onto summary of the page
    * @param maybeContent     Option onto content
    * @param maybePublishDate Option onto publication date
    * @param maybeBreadCrumbs Option onto bread crumbs
    * @param maybeImageSrc    Option onto the source of an image
    * @return Entry for url page
    */
  private def createUrlEntry(
      url: String,
      sourceId: String,
      maybeTitle: Option[String],
      maybeSubTitle: Option[String],
      maybeSummary: Option[String],
      maybeContent: Option[String],
      maybePublishDate: Option[String],
      maybeBreadCrumbs: Option[String],
      maybeImageSrc: Option[String]
  ): SelectionBuilder[RootMutation, Option[Extractor.EntryView]] = {
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          title = maybeTitle,
          subTitle = maybeSubTitle,
          summary = maybeSummary,
          content = maybeContent,
          image = maybeImageSrc,
          url = Some(url),
          `type` = Some(EntryTypeType.url),
          publishDate = maybePublishDate,
          source = Some(
            SourceRelateToOneInput(
              connect = Some(
                SourceWhereUniqueInput(
                  sourceId
                )
              )
            )
          )
        )
      )
    )(
      Entry.view()(
        CloudinaryImage_File.view(),
        Tag.view(Language.view, CloudinaryImage_File.view()),
        _QueryMeta.view,
        Language.view,
        Source.view(GeoLocation.view(LocationGoogle.view))
      )
    )
  }

  /**
    * Build an entry with extracted page information for a video based entry
    *
    * @param pageDoc    Page document
    * @param url        Url of the page
    * @param selectors  Applicable selectors
    * @param sourceId   Id of source
    * @return Applicable [[caliban.client.SelectionBuilder]] for the target mutation
    */
  private def buildVideoEntry(
      pageDoc: Document,
      url: String,
      selectors: Selectors,
      sourceId: String
  ): SelectionBuilder[RootMutation, Option[Extractor.EntryView]] = {
    extractVideoViewInformation(pageDoc, selectors) match {
      case VideoViewInformation(
          title,
          subTitle,
          summary,
          content,
          publishDate,
          breadCrumbs,
          videoSrc
          ) =>
        createVideoEntry(
          url,
          sourceId,
          title,
          subTitle,
          summary,
          content,
          publishDate,
          breadCrumbs,
          videoSrc
        )
    }
  }

  /**
    * Extract the needed information from page document
    *
    * @param pageDoc    Page document to use
    * @param selectors  Selectors to use
    * @return Gathered information for video page
    */
  private def extractVideoViewInformation(
      pageDoc: Document,
      selectors: Selectors
  ): VideoViewInformation = VideoViewInformation(
    title = pageDoc >?> text(selectors.title),
    subTitle = selectors.subtitle.flatMap(pageDoc >?> text(_)),
    summary = selectors.summary.flatMap(pageDoc >?> text(_)),
    content = pageDoc >?> text(selectors.content),
    publishDate = selectors.date.flatMap(pageDoc >?> text(_)),
    breadCrumbs = selectors.breadcrumb.flatMap(pageDoc >?> text(_)),
    videoSrc =
      selectors.video.flatMap(pageDoc >?> element(_) >> attr("src")("source"))
  )

  /**
    * Actually building the entry with all needed information
    *
    * @param url              Url of page
    * @param sourceId         Source identifier
    * @param maybeTitle       Option onto title
    * @param maybeSubTitle    Option onto subtitle
    * @param maybeSummary     Option onto summary of the page
    * @param maybeContent     Option onto content
    * @param maybePublishDate Option onto publication date
    * @param maybeBreadCrumbs Option onto bread crumbs
    * @param maybeVideoSrc    Option onto the source of an image
    * @return Entry for url page
    */
  private def createVideoEntry(
      url: String,
      sourceId: String,
      maybeTitle: Option[String],
      maybeSubTitle: Option[String],
      maybeSummary: Option[String],
      maybeContent: Option[String],
      maybePublishDate: Option[String],
      maybeBreadCrumbs: Option[String],
      maybeVideoSrc: Option[String]
  ): SelectionBuilder[RootMutation, Option[Extractor.EntryView]] = {
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          title = maybeTitle,
          subTitle = maybeSubTitle,
          summary = maybeSummary,
          content = maybeContent,
          image = maybeVideoSrc,
          url = Some(url),
          `type` = Some(EntryTypeType.video),
          publishDate = maybePublishDate,
          source = Some(
            SourceRelateToOneInput(
              connect = Some(
                SourceWhereUniqueInput(
                  sourceId
                )
              )
            )
          )
        )
      )
    )(
      Entry.view()(
        CloudinaryImage_File.view(),
        Tag.view(Language.view, CloudinaryImage_File.view()),
        _QueryMeta.view,
        Language.view,
        Source.view(GeoLocation.view(LocationGoogle.view))
      )
    )
  }
}
