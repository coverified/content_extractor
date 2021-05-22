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
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}

class AnalyzerSpec extends ZioSpec with ProfileConfigHelper {
  "Given an analyzer" when {
    "determining the page type" should {
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

      val pathMatches = PrivateMethod[Boolean](Symbol("pathMatches"))

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
