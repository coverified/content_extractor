/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import info.coverified.extractor.ArgsParser.Args
import info.coverified.extractor.exceptions.ConfigException
import info.coverified.test.scalatest.UnitSpec
import org.scalatest.Inside.inside
import sttp.client3.UriContext

import scala.util.{Failure, Success}

class ConfigSpec extends UnitSpec {
  "A content extractor configuration" when {
    "parsed from CLI arguments" should {
      "fail on missing entries" in {
        val maliciousArgs = Table(
          "args",
          Args(None, None),
          Args(Some("foo"), None),
          Args(None, Some("bar"))
        )

        forAll(maliciousArgs) { args =>
          Config.fromArgs(args) match {
            case Failure(ConfigException(msg, _)) =>
              msg shouldBe "Cannot build config from args, as some parameter is missing."
            case Failure(exception) =>
              fail(s"Parsing failed with wrong exception '$exception'.")
            case Success(value) =>
              fail(s"Parsing delivered '$value', although it was meant to fail")
          }
        }
      }

      "deliver proper config on proper input" in {
        inside(Config.fromArgs(Args(Some("foo"), Some("bar")))) {
          case Success(Config(apiUri, profileDirectoryPath)) =>
            apiUri shouldBe uri"foo"
            profileDirectoryPath shouldBe "bar"
          case Failure(exception) =>
            fail(
              s"Parsing was meant to pass, but failed with exception '$exception'."
            )
        }
      }
    }

    "parsed from environment variables" should {
      "succeed" in {
        inside(Config.fromEnv()) {
          case Success(Config(apiUri, profileDirectoryPath)) =>
            /* The values expected here, have to placed within the environment during the build CI-stage.
             * Cf. .gitlab-ci.yml file in root directory */
            apiUri shouldBe uri"https://www.coverified.info"
            profileDirectoryPath shouldBe "in/some/directory"
          case Failure(exception) =>
            fail(
              "Parsing config from environment variables was meant to succeed, but failed.",
              exception
            )
        }
      }
    }
  }
}
