/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.config.Config
import io.sentry.{Sentry, SentryOptions}
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.{App, ExitCode, URIO}

import scala.util.{Failure, Success}

/**
  * Actually running the analysis
  *
  * @version 0.1
  * @since 26.02.21
  */
object Run extends App with LazyLogging {
  /* Set up error reporting (the DSN = Data Source Name shall be loaded from environment variables) */
  Sentry.init(
    (options: SentryOptions) => options.setEnableExternalConfiguration(true)
  )

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    logger.info("Starting extraction")

    /* Try to gather config from CLI args or environment variables */
    val config = Config.fromEnv() match {
      case Success(cfg) => cfg
      case Failure(exception) =>
        logger.info(
          "Cannot obtain config from environment variables. Trying to parse from CLI arguments. Cause:",
          exception
        )
        ArgsParser
          .parse(args.toArray)
          .flatMap(Config.fromArgs(_).toOption) match {
          case Some(cfg) => cfg
          case None =>
            throw new RuntimeException(
              "Unable to obtain config from CLI arguments."
            )
        }
    }

    /* Get all page profiles and map the corresponding hostname, it applies to, to the actual profile config */
    val hostNameToProfileConfig =
      Extractor.getAllConfigs(config.profileDirectoryPath)

    val extractor = Extractor(config, hostNameToProfileConfig)
    (for {
      _ <- extractor.extractNewOnes
        .provideCustomLayer(AsyncHttpClientZioBackend.layer())
        .delay(config.repeatDelay)
        .repeatWhile(!_)
      _ <- extractor.extractExistingOnes
        .provideCustomLayer(AsyncHttpClientZioBackend.layer())
        .delay(config.repeatDelay)
        .repeatWhile(!_)
    } yield ()).exitCode
  }
}
