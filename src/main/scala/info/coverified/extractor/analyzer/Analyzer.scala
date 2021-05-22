/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema._
import info.coverified.extractor.profile.ProfileConfig.PageType.Selectors
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.{Document, Element}

import java.time.ZonedDateTime
import com.typesafe.scalalogging.LazyLogging
import net.ruippeixotog.scalascraper.scraper.HtmlExtractor

import scala.util.{Failure, Success, Try}

/**
  * //ToDo: Class Description
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
  ) = {

    // get page doc
    Try {
      browser.get(url)
    } match {
      case Success(pageDoc: browser.DocumentType) =>
        analyze(url, pageDoc, sourceId, cfg).toOption
      case Failure(exception) =>
        logger.error(
          "Exception during analysis of url '{}': {}\nStacktrace: {}",
          url,
          exception.getMessage,
          exception.getStackTrace.toVector
        )
        None
    }
  }

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
  ) = {
    // todo use type name enums
    // TODO CK: Improve control flow at this point (nested options, unhandled exception, ...)
    Try {
      determinePageType(url, pageDoc, profileConfig).map {
        case ("url", selectors) =>
          // build url entry
          buildUrlEntry(pageDoc, url, selectors, sourceId)
        case ("video", selectors) =>
          buildVideoEntry(pageDoc, url, selectors, sourceId)
        case (unknown, _) =>
          throw new RuntimeException(s"Unknown page type: $unknown")
      }
    }
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
  ) = {
    profileConfig.profile.pageTypes
      .find(
        pageType =>
          selectorMatches(pageDoc, pageType) && pathMatches(url, pageType)
      )
      .map(pt => (pt.name, pt.selectors))
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
  ) = {
    // extract data
    val title = pageDoc >?> text(selectors.title)
    val content = pageDoc >?> text(selectors.content)
    //    val image = selectors.image.flatMap(pageDoc >?> attr("src")(_)) todo
    val subtitle = selectors.subtitle.flatMap(pageDoc >?> text(_))
    //    val breadcrumb = selectors.breadcrumb.flatMap(pageDoc >?> text(_)) todo

    // build entry mutation
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          title = title,
          subTitle = subtitle,
          content = content,
          url = Some(url),
          `type` = Some(EntryTypeType.url),
          publishDate = Some(ZonedDateTime.now().toLocalDate.toString), // todo
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
  ) = {
    // extract data
    val title = pageDoc >?> text(selectors.title)
    val content = pageDoc >?> text(selectors.content)
    //    val video = pageDoc >?> text(selectors.video) todo
    val subtitle = selectors.subtitle.flatMap(pageDoc >?> text(_))
    //    val breadcrumb = selectors.breadcrumb.flatMap(pageDoc >?> text(_)) todo
    //    val date = selectors.date.flatMap(pageDoc >?> text(_)) // todo

    // build entry mutation
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          title = title,
          subTitle = subtitle,
          content = content,
          url = Some(url),
          `type` = Some(EntryTypeType.video),
          publishDate = Some(ZonedDateTime.now().toLocalDate.toString), // todo
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
