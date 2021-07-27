/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import info.coverified.extractor.ArgsParser.Args
import info.coverified.test.scalatest.UnitSpec
import org.scalatest.Inside.inside
import sttp.client3.UriContext

import java.time.{Duration, ZoneId}
import scala.util.{Failure, Success}

class ConfigSpec extends UnitSpec {
  "A content extractor configuration" when {
    "parsed from CLI arguments" should {
      "deliver proper config on proper input" in {
        inside(
          Config
            .fromArgs(
              Args(
                "https://www.coverified.info",
                "authSecret",
                "in/some/directory",
                Some(36),
                Some(200),
                Some(20),
                Some("userAgent"),
                Some(90),
                Some("yyyy"),
                Some("Europe/Berlin")
              )
            )
        ) {
          case Success(
              Config(
                userAgent,
                browseTimeout,
                targetDateTimePattern,
                targetTimeZone,
                apiUri,
                authSecret,
                profileDirectoryPath,
                reAnalysisInterval,
                workerPoolSize,
                repeatDelay
              )
              ) =>
            userAgent shouldBe "userAgent"
            browseTimeout shouldBe Duration.ofSeconds(90L)
            targetDateTimePattern shouldBe "yyyy"
            targetTimeZone shouldBe ZoneId.of("Europe/Berlin")
            apiUri shouldBe uri"https://www.coverified.info"
            authSecret shouldBe "authSecret"
            profileDirectoryPath shouldBe "in/some/directory"
            reAnalysisInterval shouldBe Duration.ofHours(36L)
            workerPoolSize shouldBe 200
            repeatDelay shouldBe Duration.ofSeconds(20L)
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
          case Success(
              Config(
                userAgent,
                browseTimeout,
                targetDateTimePattern,
                targetTimeZone,
                apiUri,
                authSecret,
                profileDirectoryPath,
                reAnalysisInterval,
                workerPoolSize,
                repeatDelay
              )
              ) =>
            /* The values expected here, have to placed within the environment during the build CI-stage.
             * Cf. .gitlab-ci.yml file in root directory */
            userAgent shouldBe "userAgent"
            browseTimeout shouldBe Duration.ofSeconds(90L)
            targetDateTimePattern shouldBe "yyyy"
            targetTimeZone shouldBe ZoneId.of("Europe/Berlin")
            apiUri shouldBe uri"https://www.coverified.info"
            authSecret shouldBe "authSecret"
            profileDirectoryPath shouldBe "in/some/directory"
            reAnalysisInterval shouldBe Duration.ofHours(36L)
            workerPoolSize shouldBe 200
            repeatDelay shouldBe Duration.ofSeconds(20L)
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
