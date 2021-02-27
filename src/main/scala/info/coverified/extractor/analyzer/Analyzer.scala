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
import net.ruippeixotog.scalascraper.model.Document

import java.time.ZonedDateTime

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 26.02.21
  */
object Analyzer {

  def run(
      url: String,
      sourceId: String,
      cfg: ProfileConfig,
      browser: Browser = JsoupBrowser()
  ) = {

    // get page doc
    val pageDoc = browser.get(url)

    // extract data using matching page type
    // todo use type name enums
    val pageTypeName = cfg.profile.pageTypes
      .find(pageType => {
        val selectorFits = pageType.condition.selector.forall(selector => {
          pageDoc >/~ validator(elementList(selector))(_.nonEmpty) match {
            case Left(_)  => false
            case Right(_) => true
          }
        }) // if selector is not set, it is always true

        val pathFits = pageType.condition.path.forall(url.contains(_)) // if path is not set, it is always true

        selectorFits && pathFits
      })
      .map(pt => (pt.name, pt.selectors))

    val res = pageTypeName.map {
      case ("url", selectors) =>
        // build url entry
        buildUrlEntry(pageDoc, url, selectors, sourceId)
      case ("video", selectors) =>
        buildVideoEntry(pageDoc, url, selectors, sourceId)
      case (unknown, _) =>
        throw new RuntimeException(s"Unknown page type: $unknown")
    }

    res
  }

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
