/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.ArgsParser.Args
import info.coverified.extractor.config.Config
import sttp.client3.UriContext
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.{App, ExitCode, URIO, ZIO}

import java.io.File
import scala.util.{Failure, Success}

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 26.02.21
  */
object Run extends App with LazyLogging {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    // get args
    val (extractor, configFolderPath): (Extractor, String) = {
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
      (Extractor(uri"${config.apiUrl}"), config.profileDirectoryPath)
    }

    // debug
    val configs = extractor.getAllConfigs(new File(configFolderPath))

    val extractorRun = for {
      urls <- extractor.getAllUrls
      _ <- ZIO.collectAll(
        urls.flatMap(
          urlView =>
            urlView.url.zip(urlView.source).flatMap {
              case (urlString, source) =>
                extractor
                  .getProfile4Url(urlString, configs)
                  .flatMap(
                    profileCfg =>
                      extractor.getMutations(urlString, source.id, profileCfg)
                  )
            }
        )
      )
    } yield ()

    extractorRun.provideCustomLayer(AsyncHttpClientZioBackend.layer()).exitCode

  }

}
