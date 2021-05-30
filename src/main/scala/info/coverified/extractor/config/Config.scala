/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import info.coverified.extractor.ArgsParser.Args
import info.coverified.extractor.exceptions.ConfigException
import sttp.client3.UriContext
import sttp.model.Uri

import java.time.Duration
import scala.util.{Failure, Success, Try}

/**
  * Configuring the content extraction service
  *
  * @param apiUri                URI of the API that needs to be queried for the sites to extract content from
  * @param profileDirectoryPath  Directory path, where to find the site profiles
  * @param reAnalysisInterval    Interval, after which the content may be analyzed once again
  */
final case class Config(
    apiUri: Uri,
    profileDirectoryPath: String,
    reAnalysisInterval: Duration
)

object Config {
  def apply(
      apiUrl: String,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration
  ): Config =
    new Config(uri"$apiUrl", profileDirectoryPath, reAnalysisInterval)

  private val API_URL_KEY = "EXTRACTOR_API_URL"
  private val PROFILE_DIRECTORY_PATH = "EXTRACTOR_PAGE_PROFILE_PATH"
  private val RE_ANALYSIS_INTERVAL = "RE_ANALYSIS_INTERVAL"

  /**
    * Build config from parsed CLI input
    *
    * @param args The parsed input
    * @return A try to create a [[Config]]
    */
  def fromArgs(args: Args): Try[Config] = args match {
    case Args(
        Some(apiUrl),
        Some(pageProfileFolderPath),
        Some(reAnalyzeInterval)
        ) =>
      Success(
        Config(
          apiUrl,
          pageProfileFolderPath,
          Duration.ofHours(reAnalyzeInterval.toLong)
        )
      )
    case _ =>
      Failure(
        ConfigException(
          "Cannot build config from args, as some parameter is missing."
        )
      )
  }

  /**
    * Trying to get the config from environment variables
    *
    * @return A try to create a [[Config]]
    */
  def fromEnv(): Try[Config] =
    for {
      apiUrl <- fromEnv(API_URL_KEY)
      profileDirectory <- fromEnv(PROFILE_DIRECTORY_PATH)
      reAnalysisInterval <- fromEnv(RE_ANALYSIS_INTERVAL)
    } yield {
      Config(
        apiUrl,
        profileDirectory,
        Duration.ofHours(reAnalysisInterval.toLong)
      )
    }

  /**
    * Attempt to obtain an entry from environment variables
    *
    * @param key The key to get
    * @return The value
    */
  private def fromEnv(key: String): Try[String] = {
    sys.env.get(key) match {
      case Some(value) => Success(value)
      case None =>
        Failure(
          ConfigException(s"Cannot find '$key' within environment variables.")
        )
    }
  }
}
