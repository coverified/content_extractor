/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.Selectors
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.{Document, ElementNode, TextNode}
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.analyzer.EntryInformation.RawEntryInformation
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig.PageType
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import net.ruippeixotog.scalascraper.scraper.HtmlValidator
import org.jsoup.Jsoup

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.temporal.ChronoField._
import java.time.{
  DateTimeException,
  Duration,
  LocalDate,
  LocalDateTime,
  ZoneId,
  ZonedDateTime
}
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
  private val BROWSE_TIME_OUT: Duration = Duration.ofMillis(30000L)

  private val ISO_DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ssXXX"
  private val TARGET_TIME_ZONE = ZoneId.of("UTC")

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
    * @param url Url location of the web page
    * @return The content, that can be scraped later
    */
  def queryUrl(url: String): Try[JsoupDocument] = Try {
    JsoupDocument(
      Jsoup
        .connect(url)
        .ignoreContentType(false)
        .userAgent(USER_AGENT)
        .timeout(BROWSE_TIME_OUT.toMillis.toInt)
        .followRedirects(true)
        .execute()
        .parse()
    )
  }

  /**
    * Analyze the given page document and extract information
    *
    * @param url           Url of page
    * @param urlId         Identifier of the url entry in data base
    * @param pageDoc       Page document
    * @param profileConfig Applicable profile config for this page
    * @return A trial onto the needed information
    */
  private def analyze(
      url: String,
      urlId: String,
      pageDoc: JsoupDocument,
      profileConfig: ProfileConfig
  ): Try[RawEntryInformation] =
    getSelectors(url, pageDoc, profileConfig).flatMap(
      extractInformation(pageDoc, _, url)
    )

  /**
    * Determine the page type (in terms of it's "name") as well as the associated selectors for this type of document
    *
    * @param url           Url of page
    * @param pageDoc       Page document
    * @param profileConfig Applicable profile configuration
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
    * @param pageDoc   Page document
    * @param selectors Selectors to use
    * @param url       The corresponding url (only for debugging purposes)
    * @return Needed information
    */
  private def extractInformation(
      pageDoc: JsoupDocument,
      selectors: Selectors,
      url: String
  ): Try[RawEntryInformation] =
    Try {
      RawEntryInformation(
        pageDoc >> text(selectors.title),
        selectors.summary.flatMap(pageDoc >?> text(_)),
        pageDoc >?> text(selectors.content.selector),
        selectors.date.flatMap(extractDate(pageDoc, _, url))
      )
    } match {
      case success @ Success(_) => success
      case Failure(nse: NoSuchElementException) =>
        Failure(
          AnalysisException(
            s"Unable to extract mandatory title from web page @ url '$url'!",
            nse
          )
        )
      case Failure(ex) =>
        Failure(
          AnalysisException(
            s"Unknown exception during information extraction from url '$url'.",
            ex
          )
        )
    }

  /**
    * Extract the date information from web page document. If desired, it is first attempted to get it from Json LD
    * information and then subsequently from entry attribute and entry content. If applicable, a regex is used to clean
    * up the information.
    *
    * @param document   Web page document
    * @param dateConfig Configuration on how to find the String
    * @param url        The corresponding url (only for debugging purposes)
    * @return A trial to get the information
    */
  def extractDate(
      document: JsoupDocument,
      dateConfig: ProfileConfig.PageType.Selectors.Date,
      url: String
  ): Option[String] =
    /* If desired, attempt to get date time information from JSON-LD object */
    getDateTimeString(document, dateConfig, url)
      .flatMap {
        case (rawDateTimeString, dateTimeFormat) =>
          /* Date time string and format are extracted. If applicable, try to apply a regex to narrow the input */
          applyDateTimeRegex(rawDateTimeString, dateConfig.pattern)
            .map(
              processedDateTimeString =>
                (processedDateTimeString, dateTimeFormat)
            )
      }
      .flatMap {
        case (dateTimeString, dateTimeFormat) =>
          /* Re-Format the date time string, so that it matches the ISO format */
          reformatDateTimePattern(
            dateTimeString,
            dateTimeFormat,
            ZoneId.of(dateConfig.defaultZoneId)
          ) match {
            case success @ Success(_) => success
            case Failure(exception: DateTimeParseException) =>
              throw AnalysisException(
                s"Parsing of date time string '$dateTimeString' failed. Format string was '$dateTimeFormat'. Source url: '$url'",
                exception
              )
            case Failure(ex) =>
              Failure(
                AnalysisException(
                  s"Unknown exception during re-formatting of date time string. dateTimeString = '$dateTimeString', format = '$dateTimeFormat', url = '$url'",
                  ex
                )
              )
          }
      } match {
      case Success(dateTimeString) =>
        Some(dateTimeString)
      case Failure(exception) =>
        logger.error(
          "Extraction of date time information failed due to the following reason. Leave this information out.",
          exception
        )
        None
    }

  /**
    * Get date time string and matching format from page document.
    *
    * @param document   Web page document
    * @param dateConfig Configuration on how to find the String
    * @param url        The corresponding url (only for debugging purposes)
    * @return A trial to get the information
    */
  def getDateTimeString(
      document: JsoupDocument,
      dateConfig: ProfileConfig.PageType.Selectors.Date,
      url: String
  ): Try[(String, String)] =
    if (dateConfig.tryJsonLdFirst) {
      JsonLD.publishDate(document) match {
        case Success(dateTimeString) =>
          Success(dateTimeString, ISO_DATE_TIME_PATTERN)
        case Failure(exception) =>
          logger.warn(
            s"Getting date time information from JSON LD @ '$url' failed with following exception. Try to get from selected element.",
            exception
          )
          getDateTimeStringFromElement(document, dateConfig, url).map(
            (_, dateConfig.format)
          )
      }
    } else {
      getDateTimeStringFromElement(document, dateConfig, url).map(
        (_, dateConfig.format)
      )
    }

  /**
    * Extract the string, that shall contain the date time, from the element attribute. If that fails, try to get it
    * from the content itself.
    *
    * @param document   Web page document
    * @param dateConfig Configuration on how to find the String
    * @param url        The corresponding url (only for debugging purposes)
    * @return A trial to get the information
    */
  def getDateTimeStringFromElement(
      document: Document,
      dateConfig: ProfileConfig.PageType.Selectors.Date,
      url: String
  ): Try[String] = {
    dateConfig.attributeVal match {
      case Some(attribute) =>
        Try(document >> element(dateConfig.selector)).flatMap {
          dateTimeElement =>
            if (dateTimeElement.hasAttr(attribute))
              Success(dateTimeElement.attr(attribute))
            else
              getDateTimeStringFromContent(document, dateConfig.selector, url)
        }
      case None =>
        getDateTimeStringFromContent(document, dateConfig.selector, url)
    }
  }

  /**
    * Extract the string, that shall contain the date time, from content
    *
    * @param document Web page document
    * @param selector CSS selector to apply
    * @param url      The corresponding url (only for debugging purposes)
    * @return A trial to get the information
    */
  def getDateTimeStringFromContent(
      document: Document,
      selector: String,
      url: String
  ): Try[String] = Try(document >> text(selector)) match {
    case Failure(exception: NoSuchElementException) =>
      Failure(
        AnalysisException(
          s"Cannot extract date time from content with '$selector' as selector @ url '$url'.",
          exception
        )
      )
    case failure @ Failure(_) => failure
    case success @ Success(_) => success
  }

  /**
    * Apply an possibly given regex pattern to the given raw date time string and hand back the first match.
    *
    * @param rawDateTimeString The raw date time string
    * @param pattern           The regex pattern to apply
    * @param url               The corresponding url (only for debugging purposes)
    * @return An attempt to get the information.
    */
  def applyDateTimeRegex(
      rawDateTimeString: String,
      pattern: Option[String],
      url: String
  ): Try[String] =
    pattern
      .map { pattern =>
        pattern.r.findFirstIn(rawDateTimeString) match {
          case Some(matchFound) => Success(matchFound)
          case None =>
            Failure(
              AnalysisException(
                s"Application of regex pattern '$pattern' onto '$rawDateTimeString' failed. Source url: '$url'."
              )
            )
        }
      }
      .getOrElse(Success(rawDateTimeString))

  /**
    * Bring an arbitrary date time pattern to an ISO date time pattern. The input string is parsed with it's given time
    * zone information and transferred to UTC. If no time zone information is available, we use fall back information.
    * If no time information is given at all, set it to the beginning of the day.
    *
    * @param dateTimeString The input string
    * @param dateTimeFormat The matching format
    * @return A string in iso format
    */
  def reformatDateTimePattern(
      dateTimeString: String,
      dateTimeFormat: String,
      fallBackZone: ZoneId = ZoneId.of("Europe/Berlin")
  ): Try[String] =
    Try {
      val dateTimeFormatter = DateTimeFormatter.ofPattern(dateTimeFormat)
      val temporalAccessor = dateTimeFormatter.parse(dateTimeString)

      val atLeastOneOfThatFieldsForTime =
        Seq(HOUR_OF_DAY, HOUR_OF_AMPM, CLOCK_HOUR_OF_DAY, CLOCK_HOUR_OF_AMPM)
      if (atLeastOneOfThatFieldsForTime.exists(temporalAccessor.isSupported(_))) {
        /* The string contains time information. Try to figure out, in which time zone the time is given. If some is,
         * apparent, take that for parsing, otherwise use the fall back one. */
        val timeZone = Try {
          temporalAccessor.query(ZoneId.from(_))
        }.getOrElse {
          logger.debug(
            s"Unable to get zone id from '$dateTimeString'. Take target time zone '$fallBackZone'."
          )
          fallBackZone
        }

        LocalDateTime.from(temporalAccessor).atZone(timeZone)
      } else {
        /* The string does not contain time information. It is set to 00:00 h */
        LocalDate.from(temporalAccessor).atStartOfDay(fallBackZone)
      }
    }.map(_.withZoneSameInstant(TARGET_TIME_ZONE))
      .map(_.format(DateTimeFormatter.ofPattern(ISO_DATE_TIME_PATTERN)))

  /**
    * Extract content from web page under consideration of exclude selectors, that are meant to odd out child elements,
    * that are to be excluded from real content
    *
    * FIXME: Test
    * FIXME: Private
    *
    * @param document         The total web page's document
    * @param contentSelector  Selector to find the overall content of the page
    * @param excludeSelectors Selectors for the elements to be excluded
    * @return
    */
  def extractContent(
      document: Document,
      contentSelector: String,
      excludeSelectors: Seq[String]
  ): Option[String] =
    document >?> element(contentSelector)
      .map {
        _.childNodes
          .map {
            /* Check the single children. If it's plain text, just go over it, if it's an element, check, if any of the
             * exclude selectors applies */
            case TextNode(text)       => text
            case ElementNode(element) =>
              /* Parse the outer html code of the element, so that the validation can be performed */
              val elementDocument =
                JsoupDocument(Jsoup.parse(element.outerHtml))
              checkDocument(elementDocument, excludeSelectors).getOrElse {
                logger.debug(
                  "Document is neglected due to exclude selector. Apply empty string."
                )
                ""
              }
          }
          .mkString(" ")
      }
      .map { joined =>
        /* Trim leading and trailing whitespaces */
        "^ +| +$".r.replaceAllIn(joined, "")
      }
      .map { joinedAndTrimmed =>
        /* Remove duplicated white spaces */
        " {2,}".r.replaceAllIn(joinedAndTrimmed, " ")
      }

  /**
    * Check a document against exclude selectors. If none of them matches, it returns the text content of the document
    *
    * FIXME: Test
    * FIXME: Private
    *
    * @param document         The document to check
    * @param excludeSelectors Exclude selectors
    * @return An [[Option]] onto the matching content
    */
  def checkDocument(
      document: Document,
      excludeSelectors: Seq[String]
  ): Option[String] = excludeSelectors.foldLeft(Option.apply("")) {
    case (Some(_), excludeSelector) =>
      document >/~ HtmlValidator(elements(excludeSelector))(_.isEmpty) match {
        case Left(()) =>
          logger.debug(
            "Detected content to be excluded based on '{}'.",
            excludeSelector
          )
          None
        case Right(passedDocument) =>
          Some(passedDocument >> allText)
      }
    case (None, _) => None
  }
}
