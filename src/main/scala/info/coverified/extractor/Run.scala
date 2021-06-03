/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.config.Config
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.{App, ExitCode, URIO, ZIO}

import scala.util.{Failure, Success}

/**
  * Actually running the analysis
  *
  * @version 0.1
  * @since 26.02.21
  */
object Run extends App with LazyLogging {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    logger.info("Starting extraction")

    /* Try to gather config from CLI args or environment variables */
    val config = ArgsParser
      .parse(args.toArray)
      .flatMap(Config.fromArgs(_).toOption) match {
      case Some(config) => config
      case None =>
        logger.info(
          "Trying to get configuration from environment variables ... "
        )
        Config.fromEnv() match {
          case Success(config) => config
          case Failure(exception) =>
            throw new RuntimeException(
              "Config parameters missing!",
              exception
            )
        }
    }

    Extractor(config)
      .buildExtractionEffect()
      .provideCustomLayer(AsyncHttpClientZioBackend.layer())
      .exitCode
  }
}
