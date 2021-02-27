/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.ArgsParser.Args
import info.coverified.profile.ProfileConfig
import sttp.client3.UriContext
import sttp.client3.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.{App, ExitCode, URIO, ZIO}

import java.io.File

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 26.02.21
  */
object Run extends App with LazyLogging {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    // get args
    val (extractor, configFolderPath): (Extractor, String) =
      ArgsParser
        .parse(args.toArray)
        .flatMap {
          case Args(Some(apiUrl), Some(pageProfileFolderPath)) =>
            Some((Extractor(uri"$apiUrl"), pageProfileFolderPath))
          case _ =>
            logger.info(
              "Trying to get configuration from environment variables ... "
            )
            None
        }
        .getOrElse(
          Option(sys.env("EXTRACTOR_API_URL"))
            .zip(Option(sys.env("EXTRACTOR_PAGE_PROFILE_PATH"))) match {
            case Some((apiUrl, pageProfileFolderPath)) =>
              (Extractor(uri"$apiUrl"), pageProfileFolderPath)
            case None =>
              throw new RuntimeException(
                "Config parameters missing!"
              )
          }
        )

    // debug
    val configs = extractor.getAllConfigs(new File(configFolderPath))

    val extractorRun = for {
      urls <- extractor.getAllUrls
      //      existingEntries <- extractor.getExistingEntries
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
