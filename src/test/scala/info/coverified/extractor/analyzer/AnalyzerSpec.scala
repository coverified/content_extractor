/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}

class AnalyzerSpec extends ZioSpec with ProfileConfigHelper {
  "Given an analyzer" when {
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
