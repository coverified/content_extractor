/**
 * © 2021. CoVerified GmbH
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
                apiUrl = "https://www.coverified.info",
                authSecret = "authSecret",
                pageProfileDirectoryPath = "in/some/directory",
                reAnalysisInterval = Some(36),
                workerPoolSize = Some(200),
                repeatDelay = Some(20),
                maxRetries = Some(42),
                userAgent = Some("userAgent"),
                browseTimeout = Some(90),
                targetDateTimePattern = Some("yyyy"),
                targetTimeZone = Some("Europe/Berlin")
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
                repeatDelay,
                maxRetries
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
            maxRetries shouldBe 42
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
                repeatDelay,
                maxRetries
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
            maxRetries shouldBe 42
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
