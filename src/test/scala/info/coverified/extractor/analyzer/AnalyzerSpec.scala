/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.{
  Condition,
  Selectors
}
import info.coverified.extractor.profile.ProfileConfig.Profile
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}
import net.ruippeixotog.scalascraper.model.Document

class AnalyzerSpec extends ZioSpec with ProfileConfigHelper {
  "Given an analyzer" when {
    "determining the page type" should {
      val validPageDoc: Document = MockBrowser.MockDoc()

      val validPageType = ProfileConfig.PageType(
        condition = Condition(
          path = Some("https://wwww.coverified.info/impressum"),
          selector = None
        ),
        examples = List("a", "b"),
        name = "test_type",
        selectors = Selectors(
          audio = None,
          breadcrumb = None,
          content = "no content",
          date = None,
          image = None,
          subtitle = None,
          summary = None,
          title = "fancy title",
          video = None
        )
      )

      val selectorMatches = PrivateMethod[Boolean](Symbol("selectorMatches"))

      "refuse matching selector, if selector does not lead to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("this won't work"))
        )
        Analyzer invokePrivate selectorMatches(
          validPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe false
      }

      "confirm matching selector, if selector leads to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some(MockBrowser.validSelector))
        )
        Analyzer invokePrivate selectorMatches(
          validPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      "confirm matching selector, if no selector is set" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = None)
        )
        Analyzer invokePrivate selectorMatches(
          validPageDoc,
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
          "https://wwww.coverified.info/impressum/subpage",
          pageTypeWithPath
        ) shouldBe false
      }

      "confirm matching path, if path actually matches" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = Some("https://wwww.coverified.info/impressum"))
        )
        Analyzer invokePrivate pathMatches(
          "https://wwww.coverified.info/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      "confirm matching path, if path is not set" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = None)
        )
        Analyzer invokePrivate pathMatches(
          "https://wwww.coverified.info/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      val determinePageType =
        PrivateMethod[Option[(String, Selectors)]](Symbol("determinePageType"))
      val validPath = Some("https://wwww.coverified.info/impressum")

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
            Profile("https://www.coverified.info", List(pageType))
          )

          Analyzer invokePrivate determinePageType(
            "https://wwww.coverified.info/impressum/subpage",
            validPageDoc,
            profileConfig
          ) shouldBe None
        }
      }

      "succeed, if both conditions are satisfied" in {
        forAll(
          Table(
            ("selector", "path"),
            (None, None),
            (Some(MockBrowser.validSelector), None),
            (None, validPath)
          )
        ) { (selector, path) =>
          val pageType = validPageType.copy(
            condition =
              validPageType.condition.copy(path = path, selector = selector)
          )

          val profileConfig = ProfileConfig(
            Profile("https://www.coverified.info", List(pageType))
          )

          Analyzer invokePrivate determinePageType(
            "https://wwww.coverified.info/impressum/subpage",
            validPageDoc,
            profileConfig
          ) match {
            case Some((name, selectors)) =>
              name shouldBe validPageType.name
              selectors shouldBe validPageType.selectors
            case None => fail("Page type should be determined.")
          }
        }
      }
    }

    "running the analysis" should {
      val browser = new MockBrowser()
      val profileConfig = getConfig(MockBrowser.dislikedUrl)

      "return None, when browser fails" in {
        Analyzer.run(MockBrowser.dislikedUrl, "", profileConfig, browser) shouldBe None
      }
    }

    // TODO CK: Outcome if analysis succeeds or fails
  }
}
