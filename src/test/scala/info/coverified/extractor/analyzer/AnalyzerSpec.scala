/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.analyzer.BrowserHelper.RichDocument
import info.coverified.extractor.analyzer.ContentExtractorHelper.ExtractorTestCase
import info.coverified.extractor.analyzer.EntryInformation.RawEntryInformation
import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.Selectors.Date
import info.coverified.extractor.profile.ProfileConfig.PageType.{
  Condition,
  Selectors
}
import info.coverified.extractor.profile.ProfileConfig.Profile
import info.coverified.test.scalatest.MockBrowser.DislikeThatUrlException
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import org.jsoup.Jsoup
import org.mockito.scalatest.MockitoSugar
import org.scalatest.Inside.inside

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

class AnalyzerSpec
    extends ZioSpec
    with ProfileConfigHelper
    with BrowserHelper
    with MockitoSugar
    with ContentExtractorHelper {
  "Given an analyzer" when {
    val validPageType = ProfileConfig.PageType(
      condition = Condition(
        path = Some(coverifiedUrl + "/impressum"),
        selector = None
      ),
      examples = List("a", "b"),
      name = "url",
      selectors = Selectors(
        audio = None,
        breadcrumb = Some("#breadcrumb"),
        content =
          Selectors.Content(selector = "#content", exclude_selector = None),
        date = Some(
          Date(
            tryJsonLdFirst = false,
            selector = "#publishedAt",
            format = "yyyy-MM-dd'T'HH:mm:ssX",
            attributeVal = None,
            pattern = None,
            defaultZoneId = "Europe/Berlin"
          )
        ),
        image = Some("#pic"),
        subtitle = Some("#subtitle"),
        summary = Some("#summary"),
        title = "#title",
        video = Some("#vid"),
        tags = Some(".tag")
      )
    )

    "getting applicable selectors" should {
      val selectorMatches = PrivateMethod[Boolean](Symbol("selectorMatches"))

      "refuse matching selector, if selector does not lead to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("this won't work"))
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc.toScraperDoc,
          pageTypeWithoutSelectors
        ) shouldBe false
      }

      "confirm matching selector, if selector leads to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("title"))
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc.toScraperDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      "confirm matching selector, if no selector is set" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = None)
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc.toScraperDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      val pathMatches = PrivateMethod[Boolean](Symbol("pathMatches"))

      "deny matching path, if path does not match" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = Some("https://wwww.ard.de"))
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe false
      }

      "confirm matching path, if path actually matches" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = Some(coverifiedUrl + "/impressum"))
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      "confirm matching path, if path is not set" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = None)
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      val getSelectors =
        PrivateMethod[Try[Selectors]](Symbol("getSelectors"))
      val validPath = Some(coverifiedUrl + "/impressum")

      "fail, if one of either conditions is not satisfied" in {
        forAll(
          Table(
            ("selector", "path"),
            (Some("some invalid selector"), Some("some invalid path")),
            (Some(MockBrowser.validSelector), Some("some invalid path")),
            (Some("some invalid selector"), validPath)
          )
        ) { (selector, path) =>
          val pageType = validPageType.copy(
            condition =
              validPageType.condition.copy(path = path, selector = selector)
          )

          val profileConfig = ProfileConfig(
            Profile(coverifiedUrl, List(pageType))
          )

          Analyzer invokePrivate getSelectors(
            coverifiedUrl + "/impressum/subpage",
            validUrlPageDoc.toScraperDoc,
            profileConfig
          ) match {
            case Failure(exception: AnalysisException) =>
              exception.msg shouldBe s"Unable to gather profile config for url '${coverifiedUrl + "/impressum/subpage"}'."
            case Failure(exception) =>
              fail("Failed with wrong exception.", exception)
            case Success(_) =>
              fail("Determination of page type was meant to fail.")
          }
        }
      }

      "succeed, if both conditions are satisfied" in {
        forAll(
          Table(
            ("selector", "path"),
            (None, None),
            (Some("title"), None),
            (None, validPath)
          )
        ) { (selector, path) =>
          val pageType = validPageType.copy(
            condition =
              validPageType.condition.copy(path = path, selector = selector)
          )

          val profileConfig = ProfileConfig(
            Profile(coverifiedUrl, List(pageType))
          )

          Analyzer invokePrivate getSelectors(
            coverifiedUrl + "/impressum/subpage",
            validUrlPageDoc.toScraperDoc,
            profileConfig
          ) match {
            case Success(selectors) =>
              selectors shouldBe validPageType.selectors
            case Failure(exception) =>
              fail(
                "Page type should be determined. Failed with exception.",
                exception
              )
          }
        }
      }
    }

    "extracting the date information" should {
      val selector = "#date"
      val ISO_DATE_TIME_PATTERN =
        PrivateMethod[String](Symbol("ISO_DATE_TIME_PATTERN"))
      val url = "test.url"

      "correctly parse a valid ISO date time string to ZonedDateTime" in {
        val input = "2019-06-27T22:00:00Z"
        val expected =
          ZonedDateTime.of(2019, 6, 27, 22, 0, 0, 0, ZoneId.of("Z"))

        val dateTimePattern = Analyzer invokePrivate ISO_DATE_TIME_PATTERN()

        val actual = ZonedDateTime.parse(
          input,
          DateTimeFormatter.ofPattern(dateTimePattern)
        )
        actual shouldBe expected
      }

      "fail, if date time cannot be obtained from content" in {
        val document = JsoupDocument(Jsoup.parse("""
            |<html>
            | <body>
            |   Nothing interesting here.
            | </body>
            |</html>
            |""".stripMargin))

        Analyzer.getDateTimeStringFromContent(document, selector, url) match {
          case Failure(_) => succeed
          case Success(_) =>
            fail("Extraction of information was meant to fail, but passed.")
        }
      }

      "succeed, if date time can be obtained from content" in {
        val expected = "2021-07-20T23:00:00Z"
        val document = JsoupDocument(Jsoup.parse(s"""
            |<html>
            | <body>
            |   Nothing interesting here.
            |   <div id="date">$expected</div>
            | </body>
            |</html>
            |""".stripMargin))

        Analyzer.getDateTimeStringFromContent(document, selector, url) match {
          case Success(dateTimeString) => dateTimeString shouldBe expected
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get content based date time string, if element-wise receive is not desired" in {
        val expected = "2021-07-20T23:00:00Z"
        val document = JsoupDocument(Jsoup.parse(s"""
             |<html>
             | <body>
             |   Nothing interesting here.
             |   <time id="date" datetime="2021-07-20T23:05:00Z">$expected</div>
             | </body>
             |</html>
             |""".stripMargin))

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = None,
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeStringFromElement(document, config, url) match {
          case Success(dateTimeString) => dateTimeString shouldBe expected
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get content based date time string, if element-wise receive fails" in {
        val expected = "2021-07-20T23:00:00Z"
        val document = JsoupDocument(Jsoup.parse(s"""
             |<html>
             | <body>
             |   Nothing interesting here.
             |   <time id="date" thattime="2021-07-20T23:05:00Z">$expected</div>
             | </body>
             |</html>
             |""".stripMargin))

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = None,
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeStringFromElement(document, config, url) match {
          case Success(dateTimeString) => dateTimeString shouldBe expected
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get element based date time string, if element-wise receive is desired" in {
        val expected = "2021-07-20T23:05:00Z"
        val document = JsoupDocument(Jsoup.parse(s"""
             |<html>
             | <body>
             |   Nothing interesting here.
             |   <time id="date" datetime="$expected">2021-07-20T23:00:00Z</div>
             | </body>
             |</html>
             |""".stripMargin))

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetime"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeStringFromElement(document, config, url) match {
          case Success(dateTimeString) => dateTimeString shouldBe expected
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      val fullDocument = JsoupDocument(Jsoup.parse(s"""
        |<html>
        | <head>
        |   <script type="application/ld+json">
        |{
        |	"@context":			"http://schema.org",
        |	"@type":			"Article",
        |	"datePublished":	  "2021-07-20T23:20:00+01:00",
        | "dateCreated":	    "2021-07-20T23:15:00+01:00",
        |	"dateModified":	    "2021-07-20T23:10:00+01:00"
        |}
        |</script>
        | </head>
        | <body>
        |   Nothing interesting here.
        |   <time id="date" datetime="2021-07-20T23:05:00Z">2021-07-20T23:00:00Z</div>
        | </body>
        |</html>
        |""".stripMargin))

      "get date time string from correct source, if receive from JSON-LD is desired and succeeds" in {
        val expected = "2021-07-20T23:20:00+01:00"
        val expectedDateTimePattern = Analyzer invokePrivate ISO_DATE_TIME_PATTERN()

        val config = Date(
          tryJsonLdFirst = true,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetime"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(dateTimeString) =>
            dateTimeString shouldBe (expected, expectedDateTimePattern)
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get date time string from correct source, if receive from JSON-LD is desired and fails" in {
        val fullDocument = JsoupDocument(Jsoup.parse(s"""
          |<html>
          | <body>
          |   Nothing interesting here.
          |   <time id="date" datetime="2021-07-20T23:05:00Z">2021-07-20T23:00:00Z</div>
          | </body>
          |</html>
          |""".stripMargin))

        val expected = "2021-07-20T23:05:00Z"

        val config = Date(
          tryJsonLdFirst = true,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetime"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(dateTimeString) =>
            dateTimeString shouldBe (expected, "yyyy-MM-dd'T'HH:mm:ssZ")
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get date time string from correct source, if receive from element is desired and succeeds" in {
        val expected = "2021-07-20T23:05:00Z"

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetime"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(dateTimeString) =>
            dateTimeString shouldBe (expected, "yyyy-MM-dd'T'HH:mm:ssZ")
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get date time string from correct source, if receive from element is desired and fails" in {
        val expected = "2021-07-20T23:00:00Z"

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetimer"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(dateTimeString) =>
            dateTimeString shouldBe (expected, "yyyy-MM-dd'T'HH:mm:ssZ")
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "get date time string from correct source, if receive from content is desired and succeeds" in {
        val expected = "2021-07-20T23:05:00Z"

        val config = Date(
          tryJsonLdFirst = false,
          selector = selector,
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = Some("datetime"),
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(dateTimeString) =>
            dateTimeString shouldBe (expected, "yyyy-MM-dd'T'HH:mm:ssZ")
          case Failure(_) =>
            fail("Extraction of information failed, but was meant to pass.")
        }
      }

      "fails getting date time string, if non of the fall back works" in {
        val config = Date(
          tryJsonLdFirst = false,
          selector = "some_wrong_selector",
          format = "yyyy-MM-dd'T'HH:mm:ssZ",
          attributeVal = None,
          pattern = None,
          defaultZoneId = "Europe/Berlin"
        )

        Analyzer.getDateTimeString(fullDocument, config, url) match {
          case Success(_) =>
            fail("Extraction of information was meant to fail, but succeeded.")
          case Failure(_) => succeed
        }
      }

      "hand back original string, if no regex pattern shall be applied" in {
        Analyzer.applyDateTimeRegex("20.07.2021 | Von", None, url) match {
          case Success(value) => value shouldBe "20.07.2021 | Von"
          case Failure(exception) =>
            fail("Applying regex was meant to pass, but failed.", exception)
        }
      }

      "correctly apply regex pattern" in {
        Analyzer.applyDateTimeRegex(
          "20.07.2021 | Von",
          Some("\\d{2}\\.\\d{2}\\.\\d{4}"),
          url
        ) match {
          case Success(value) => value shouldBe "20.07.2021"
          case Failure(exception) =>
            fail("Applying regex was meant to pass, but failed.", exception)
        }
      }

      "fail, if pattern does not apply" in {
        Analyzer.applyDateTimeRegex(
          "20.07.2021 | Von",
          Some("^BlaFoo\\d+"),
          url
        ) match {
          case Failure(_) => succeed
          case Success(value) =>
            fail(
              s"Application of regex was meant to fail, but succeeded with '$value'. Source url: '$url'."
            )
        }
      }

      "properly reformat a given date time string with fall back time zone" in {
        val input = "20.07.2021 11:15"
        val format = "dd.MM.yyyy HH:mm"
        val fallBackZone = ZoneId.of("Europe/Berlin")

        val expected = Success(
          LocalDateTime
            .parse(input, DateTimeFormatter.ofPattern(format))
            .atZone(fallBackZone)
            .withZoneSameInstant(ZoneId.of("UTC"))
            .format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssX"))
        )

        /* If no time zone information is provided, assume we are in UTC */
        Analyzer.reformatDateTimePattern(input, format, fallBackZone) shouldBe expected
      }

      "properly reformat date time string with 'Z' as time zone" in {
        val input = "2019-06-27T22:00:00Z"
        val expected = Success(input)
        val dateTimeFormat = Analyzer invokePrivate ISO_DATE_TIME_PATTERN()

        Analyzer.reformatDateTimePattern(input, dateTimeFormat) shouldBe expected
      }

      "properly reformat date time string with other time zone description" in {
        val input = "2019-06-27T22:00:00+01:00"
        /* The input is given in another time zone, therefore the instant is transferred to UTC. */
        val expected = Success("2019-06-27T21:00:00Z")
        val dateTimeFormat = Analyzer invokePrivate ISO_DATE_TIME_PATTERN()

        Analyzer.reformatDateTimePattern(input, dateTimeFormat) shouldBe expected
      }

      "properly reformat date time string with missing time" in {
        val input = "01.03.2021"
        val format = "dd.MM.yyyy"
        val fallBackZone = ZoneId.of("Europe/Berlin")
        val expected = Success(
          LocalDateTime
            .parse(
              input + "T00:00:00",
              DateTimeFormatter.ofPattern(format + "'T'HH:mm:ss")
            )
            .atZone(fallBackZone)
            .withZoneSameInstant(ZoneId.of("UTC"))
            .format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssX"))
        )

        Analyzer.reformatDateTimePattern(input, format, fallBackZone) shouldBe expected
      }
    }

    "building the entries with extracted information" should {
      val extractInformation =
        PrivateMethod[Try[RawEntryInformation]](Symbol("extractInformation"))
      val url = "test.url"

      "throw an AnalysisException, if the mandatory title cannot be extracted" in {
        inside(
          Analyzer invokePrivate extractInformation(
            urlPageDocWithoutTitle.toScraperDoc,
            validPageType.selectors,
            url
          )
        ) {
          case Failure(analysisException: AnalysisException) =>
            analysisException.msg shouldBe "Unable to extract mandatory title from web page @ url 'test.url'!"
            analysisException.cause
              .isInstanceOf[NoSuchElementException] shouldBe true
          case Failure(exception) =>
            fail(
              "Scraping web page without title failed with wrong exception!",
              exception
            )
          case Success(rei) =>
            fail(
              s"Extracting content from a web page without title was meant to fail, but succeeded with following information: '$rei'"
            )
        }
      }

      "extract information correctly from url page" in {
        inside(
          Analyzer invokePrivate extractInformation(
            validUrlPageDoc.toScraperDoc,
            validPageType.selectors,
            url
          )
        ) {
          case Success(
              RawEntryInformation(
                title,
                summary,
                content,
                date,
                tags
              )
              ) =>
            title shouldBe "Url page with all information available"
            summary.getOrElse(fail("Expected to get a summary.")) shouldBe "This is a summary"
            content shouldBe Some("And with all the content.")
            date shouldBe Some("2021-06-03T13:37:00Z")
            tags shouldBe Some(List("aTag", "bTag", "cTag", "dTag"))
        }
      }

      "extract information correctly from url page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractInformation(
            validUrlPageDocWithoutOptionalInformation.toScraperDoc,
            validPageType.selectors,
            url
          )
        ) {
          case Success(
              RawEntryInformation(
                title,
                summary,
                content,
                date,
                tags
              )
              ) =>
            title shouldBe "Url page with all information available"
            summary shouldBe None
            content shouldBe None
            date shouldBe None
            tags shouldBe None
        }
      }
    }

    "extracting content" should {
      val extractContent =
        PrivateMethod[Option[String]](Symbol("extractContent"))

      "properly filter out content in different scenarios" in {
        val testCases =
          Table(
            "testCase",
            testCase0,
            testCase1,
            testCase2,
            testCase3,
            testCase4,
            testCase5
          )

        forAll(testCases) {
          case ExtractorTestCase(
              rawDocument,
              expectedContent,
              contentSelector,
              excludeSelectors
              ) =>
            Analyzer invokePrivate extractContent(
              rawDocument,
              contentSelector,
              excludeSelectors
            ) match {
              case Some(content) =>
                logger.debug(
                  "\nExpected:\n\t{}\nActual:\n\t{}",
                  expectedContent,
                  content
                )
                content shouldBe expectedContent
              case None => fail("Filtering content was meant to succeed.")
            }
        }
      }
    }

    "analysing received content" should {
      val analyze = PrivateMethod[Try[RawEntryInformation]](Symbol("analyze"))

      "skip pages, that are not meant to be analyzed" in {
        val profileConfig = ProfileConfig(
          Profile(
            coverifiedUrl,
            List(
              validPageType.copy(
                condition = validPageType.condition.copy(
                  path = Some("some invalid selector"),
                  selector = Some("some invalid path")
                )
              )
            )
          )
        )

        Analyzer invokePrivate analyze(
          coverifiedUrl + "/impressum/subpage",
          coverifiedUrlId,
          validUrlPageDoc.toScraperDoc,
          profileConfig
        ) match {
          case Failure(exception: AnalysisException) =>
            exception.msg shouldBe s"Unable to gather profile config for url '${coverifiedUrl + "/impressum/subpage"}'."
          case Failure(exception) =>
            fail("Failed with wrong exception.", exception)
          case Success(_) =>
            fail("Analysis content of unsupported url was meant to fail.")
        }
      }

      "succeed, if analysis was successful" in {
        val url = coverifiedUrl
        val urlId = coverifiedUrlId
        val profileConfig = ProfileConfig(
          Profile(
            coverifiedUrl,
            List(
              validPageType.copy(
                condition = validPageType.condition.copy(path = Some(url))
              )
            )
          )
        )

        Analyzer invokePrivate analyze(
          url,
          urlId,
          validUrlPageDoc.toScraperDoc,
          profileConfig
        ) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Analysis was meant to succeed, but failed.", exception)
        }
      }
    }

    "running the analysis" should {
      val validUrl = coverifiedUrl
      val mockBrowser = new MockBrowser(Map(validUrl -> validUrlPageDoc))
      val queryUrl: String => Try[JsoupDocument] =
        (url: String) => Try(mockBrowser.get(url)).map(JsoupDocument)

      "return Failure, when browser fails" in {
        val profileConfig = getConfig(MockBrowser.dislikedUrl)
        Analyzer.run(
          MockBrowser.dislikedUrl,
          coverifiedUrlId,
          profileConfig,
          queryUrl
        ) match {
          case Failure(exception: DislikeThatUrlException) =>
            exception.msg shouldBe "I don't like that url."
          case Failure(exception) =>
            fail("Browser failed with wrong exception.", exception)
          case Success(_) => fail("Browser was meant to fail, but succeeded.")
        }
      }

      "return something, if browser does not fail and analysis does not fail" in {
        Analyzer.run(validUrl, "coverified", getConfig(validUrl), queryUrl) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Browser did not fail, but analysis failed.", exception)
        }
      }
    }
  }
}
