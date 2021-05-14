/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import info.coverified.extractor.ArgsParser.Args
import info.coverified.extractor.exceptions.ConfigException

import scala.util.{Failure, Success, Try}

/**
  * Configuring the content extraction service
  *
  * @param apiUrl                URL of the API that needs to be queried for the sites to extract content from
  * @param profileDirectoryPath  Directory path, where to find the site profiles
  */
final case class Config(apiUrl: String, profileDirectoryPath: String)

object Config {
  private val API_URL_KEY = "EXTRACTOR_API_URL"
  private val PROFILE_DIRECTORY_PATH = "EXTRACTOR_PAGE_PROFILE_PATH"

  /**
    * Build config from parsed CLI input
    *
    * @param args The parsed input
    * @return A try to create a [[Config]]
    */
  def fromArgs(args: Args): Try[Config] = {
    case Args(Some(apiUrl), Some(pageProfileFolderPath)) =>
      Success(Config(apiUrl, pageProfileFolderPath))
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
    } yield {
      Config(apiUrl, profileDirectory)
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
