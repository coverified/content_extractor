/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import info.coverified.test.scalatest.UnitSpec

class EntryInformationSpec extends UnitSpec {
  "Having entry information" when {
    "determining the content hash code" should {
      val contentHash = PrivateMethod[Int](Symbol("contentHash"))
      "lead to correct hash code" in {
        EntryInformation invokePrivate contentHash(
          "The title",
          "This summarizes everything",
          "This contains a lot.",
          "2021-07-21T22:00:00Z"
        ) shouldBe 328104163
      }

      "return a different hash code on different content" in {
        EntryInformation invokePrivate contentHash(
          "The title",
          "This summarizes everything",
          "This contains less.",
          "2021-07-21T22:00:00Z"
        ) shouldBe 781685212
      }

      "return correct hash code, if some fields have empty content" in {
        EntryInformation invokePrivate contentHash("The title", "", "", "") shouldBe 318208789
      }

      "return correct hash code from entry creation information" in {
        CreateEntryInformation(
          title = "The title",
          summary = Some("This summarizes everything"),
          content = Some("This contains a lot."),
          date = Some("2021-07-21T22:00:00Z"),
          tags = Some(List("aTag", "bTag", "cTag", "dTag")),
          eTag = None
        ).contentHash shouldBe 328104163
      }

      "return correct hash code from entry creation information with empty optionals" in {
        CreateEntryInformation(
          title = "The title",
          summary = None,
          content = None,
          date = None,
          tags = None,
          eTag = None
        ).contentHash shouldBe 318208789
      }
    }
  }
}
