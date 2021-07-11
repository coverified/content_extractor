/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.Selectors
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.Document
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.analyzer.EntryInformation.RawEntryInformation
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig.PageType
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import org.jsoup.{Connection, Jsoup}

import java.time.Duration
import scala.util.{Failure, Success, Try}

/**
  * Instance that is able to fetch information from a given url and analyse it's content with respect to a given profile
  * defined for that url
  *
  * @version 0.1
  * @since 26.02.21
  */
object Analyzer extends LazyLogging {

  private val USER_AGENT: String = "CoVerifiedBot-Extractor"
  private val BROWSE_TIME_OUT: Duration = Duration.ofMillis(5000L)

  def run(
      url: String,
      urlId: String,
      cfg: ProfileConfig,
      queryUrl: String => Try[JsoupDocument] = u => Analyzer.queryUrl(u)
  ): Try[RawEntryInformation] =
    queryUrl(url).flatMap(analyze(url, urlId, _, cfg))

  /**
    * Get the content of the web page to be reached with the current url
    *
    * @param url  Url location of the web page
    * @return The content, that can be scraped later
    */
  def queryUrl(url: String): Try[JsoupDocument] = Try {
    JsoupDocument(
      Jsoup
        .connect(url)
        .ignoreContentType(false)
        .userAgent(USER_AGENT)
        .timeout(BROWSE_TIME_OUT.toMillis.toInt)
        .followRedirects(false)
        .execute()
        .parse()
    )
  }

  /**
    * Analyze the given page document and extract information
    *
    * @param url            Url of page
    * @param urlId          Identifier of the url entry in data base
    * @param pageDoc        Page document
    * @param profileConfig  Applicable profile config for this page
    * @return A trial onto the needed information
    */
  private def analyze(
      url: String,
      urlId: String,
      pageDoc: Document,
      profileConfig: ProfileConfig
  ): Try[RawEntryInformation] =
    getSelectors(url, pageDoc, profileConfig).map(
      extractInformation(pageDoc, _)
    )

  /**
    * Determine the page type (in terms of it's "name") as well as the associated selectors for this type of document
    *
    * @param url            Url of page
    * @param pageDoc        Page document
    * @param profileConfig  Applicable profile configuration
    * @return Option onto a tuple of page type and associated selectors
    */
  private def getSelectors(
      url: String,
      pageDoc: Document,
      profileConfig: ProfileConfig
  ): Try[Selectors] =
    profileConfig.profile.pageTypes
      .find(
        pageType =>
          selectorMatches(pageDoc, pageType) && pathMatches(url, pageType)
      )
      .fold[Try[Selectors]] {
        Failure(
          AnalysisException(s"Unable to gather profile config for url '$url'.")
        )
      } {
        case PageType(_, _, _, selectors) => Success(selectors)
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
    * Extract the needed information from the page
    *
    * @param pageDoc    Page document
    * @param selectors  Selectors to use
    * @return Needed information
    */
  private def extractInformation(
      pageDoc: Document,
      selectors: Selectors
  ): RawEntryInformation =
    RawEntryInformation(
      pageDoc >?> text(selectors.title),
      selectors.summary.flatMap(pageDoc >?> text(_)),
      pageDoc >?> text(selectors.content),
      selectors.date.flatMap(pageDoc >?> text(_))
    )
}
