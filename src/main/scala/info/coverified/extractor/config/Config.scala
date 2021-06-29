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
  * @param authSecret            Secret to authenticate against API
  * @param chunkSize             Amount of urls to query at the same time
  */
final case class Config private (
    apiUri: Uri,
    profileDirectoryPath: String,
    reAnalysisInterval: Duration,
    authSecret: String,
    chunkSize: Int
)

object Config {
  object DefaultValues {
    val reAnalysisInterval: Duration = Duration.ofHours(48L)
    val chunkSize = 1000
  }

  def apply(
      apiUrl: String,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int
  ): Config =
    new Config(
      uri"$apiUrl",
      profileDirectoryPath,
      reAnalysisInterval,
      authSecret,
      chunkSize
    )

  private val API_URL_KEY = "EXTRACTOR_API_URL"
  private val PROFILE_DIRECTORY_PATH = "EXTRACTOR_PAGE_PROFILE_PATH"
  private val RE_ANALYSIS_INTERVAL = "RE_ANALYSIS_INTERVAL"
  private val AUTH_SECRET = "AUTH_SECRET"
  private val EXTRACTOR_CHUNK_SIZE = "EXTRACTOR_CHUNK_SIZE"

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
        maybeReAnalysisInterval,
        Some(authSecret),
        maybeChunkSize
        ) =>
      Success(
        Config(
          apiUrl,
          pageProfileFolderPath,
          maybeReAnalysisInterval
            .map(
              reAnalysisInterval => Duration.ofHours(reAnalysisInterval.toLong)
            )
            .getOrElse(DefaultValues.reAnalysisInterval),
          authSecret,
          maybeChunkSize.getOrElse(DefaultValues.chunkSize)
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
      authSecret <- fromEnv(AUTH_SECRET)
    } yield {
      Config(
        apiUrl,
        profileDirectory,
        sys.env
          .get(RE_ANALYSIS_INTERVAL)
          .map(
            reAnalysisInterval => Duration.ofHours(reAnalysisInterval.toLong)
          )
          .getOrElse(DefaultValues.reAnalysisInterval),
        authSecret,
        sys.env
          .get(EXTRACTOR_CHUNK_SIZE)
          .map(chunkSize => chunkSize.toInt)
          .getOrElse(DefaultValues.chunkSize)
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
