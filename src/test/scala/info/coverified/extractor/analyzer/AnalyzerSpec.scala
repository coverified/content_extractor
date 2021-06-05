/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import caliban.client.Operations.RootMutation
import caliban.client.{Argument, SelectionBuilder}
import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.{
  Condition,
  Selectors
}
import info.coverified.extractor.profile.ProfileConfig.Profile
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryCreateInput,
  UrlRelateToOneInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.{SimpleEntry, SimpleUrl}
import info.coverified.test.scalatest.MockBrowser.DislikeThatUrlException
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}
import org.mockito.scalatest.MockitoSugar
import org.scalatest.Inside.inside

import scala.util.{Failure, Success, Try}

class AnalyzerSpec
    extends ZioSpec
    with ProfileConfigHelper
    with BrowserHelper
    with MockitoSugar {
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
        content = "#content",
        date = Some("#publishedAt"),
        image = Some("#pic"),
        subtitle = Some("#subtitle"),
        summary = Some("#summary"),
        title = "#title",
        video = Some("#vid")
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
          validUrlPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe false
      }

      "confirm matching selector, if selector leads to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("title"))
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      "confirm matching selector, if no selector is set" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = None)
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc,
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
            validUrlPageDoc,
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
            validUrlPageDoc,
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

    "building the entries with extracted information" should {
      val extractInformation =
        PrivateMethod[EntryInformation](Symbol("extractInformation"))
      "extract information correctly from url page" in {
        inside(
          Analyzer invokePrivate extractInformation(
            validUrlPageDoc,
            validPageType.selectors
          )
        ) {
          case EntryInformation(
              title,
              summary,
              content
              ) =>
            title shouldBe Some("Url page with all information available")
            summary.getOrElse(fail("Expected to get a summary.")) shouldBe "This is a summary"
            content shouldBe Some("And with all the content.")
        }
      }

      "extract information correctly from url page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractInformation(
            validUrlPageDocWithoutOptionalInformation,
            validPageType.selectors
          )
        ) {
          case EntryInformation(
              title,
              summary,
              content
              ) =>
            title shouldBe Some("Url page with all information available")
            summary shouldBe None
            content shouldBe None
        }
      }

      val buildEntry = PrivateMethod[SelectionBuilder[RootMutation, Option[
        SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
      ]]](Symbol("buildEntry"))
      "creates entry correctly" in {
        val selectionBuilder = Analyzer invokePrivate buildEntry(
          "1",
          Some("Title"),
          Some("Summary"),
          Some("content")
        )

        selectionBuilder match {
          case SelectionBuilder.Field(name, _, _, arguments, _) =>
            name shouldBe "createEntry"
            arguments.size shouldBe 1
            arguments.headOption match {
              case Some(Argument(name, value)) =>
                name shouldBe "data"
                value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.name shouldBe Some("Title")
                    eci.content shouldBe Some("content")
                    eci.summary shouldBe Some("Summary")
                    eci.url shouldBe Some(
                      UrlRelateToOneInput(
                        None,
                        Some(UrlWhereUniqueInput("1")),
                        None,
                        None
                      )
                    )
                    eci.tags shouldBe None
                    eci.language shouldBe None
                    eci.hasBeenTagged shouldBe None
                  case None => fail("Data should actually contain data")
                }
              case None => fail("Expected to get one argument")
            }
          case _ => fail("Got wrong result")
        }
      }
    }

    "analysing received content" should {
      val analyze = PrivateMethod[Try[SelectionBuilder[RootMutation, Option[
        SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
      ]]]](Symbol("analyze"))

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
          validUrlPageDoc,
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
          validUrlPageDoc,
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

      "return Failure, when browser fails" in {
        val profileConfig = getConfig(MockBrowser.dislikedUrl)
        Analyzer.run(
          MockBrowser.dislikedUrl,
          coverifiedUrlId,
          profileConfig,
          mockBrowser
        ) match {
          case Failure(exception: DislikeThatUrlException) =>
            exception.msg shouldBe "I don't like that url."
          case Failure(exception) =>
            fail("Browser failed with wrong exception.", exception)
          case Success(_) => fail("Browser was meant to fail, but succeeded.")
        }
      }

      "return something, if browser does not fail and analysis does not fail" in {
        Analyzer.run(validUrl, "coverified", getConfig(validUrl), mockBrowser) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Browser did not fail, but analysis failed.", exception)
        }
      }
    }
  }
}
