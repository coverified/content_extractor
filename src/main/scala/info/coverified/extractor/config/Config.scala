/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.ArgsParser.Args
import info.coverified.extractor.exceptions.ConfigException
import sttp.client3.UriContext
import sttp.model.Uri

import java.time.{Duration, ZoneId}
import scala.util.{Failure, Success, Try}

/**
  * Configuring the content extraction service
  *
  * @param userAgent              User agent to be sent when reaching out to websites
  * @param browseTimeout          Time out, when reaching out for websites
  * @param targetDateTimePattern  The target date time pattern, in which date time information shall be sent to GraphQL
  *                               API
  * @param targetTimeZone         The target time zone, in which date time information shall be sent to GraphQL
  *                               API
  * @param apiUri                URI of the API that needs to be queried for the sites to extract content from
  * @param authSecret            Secret to authenticate against API
  * @param profileDirectoryPath  Directory path, where to find the site profiles
  * @param reAnalysisInterval    Interval, after which the content may be analyzed once again
  * @param workerPoolSize        Amount of workers to paralle handle urls in parallel
  * @param repeatDelay           Delay between two successive runs, if there are still some urls left
  * @param maxRetries            Maximum permissible amount of retries, if an url's rate limit is exceeded
  */
final case class Config private (
    userAgent: String,
    browseTimeout: Duration,
    targetDateTimePattern: String,
    targetTimeZone: ZoneId,
    apiUri: Uri,
    authSecret: String,
    profileDirectoryPath: String,
    reAnalysisInterval: Duration,
    workerPoolSize: Int,
    repeatDelay: Duration,
    maxRetries: Int
)

object Config extends LazyLogging {
  private val USER_AGENT = "USER_AGENT"
  private val BROWSE_TIMEOUT = "BROWSE_TIMEOUT"
  private val TARGET_DATE_TIME_PATTERN = "TARGET_DATE_TIME_PATTERN"
  private val TARGET_TIME_ZONE = "TARGET_TIME_ZONE"
  private val API_URL = "API_URL"
  private val AUTH_SECRET = "AUTH_SECRET"
  private val PROFILE_DIRECTORY_PATH = "PAGE_PROFILE_DIRECTORY_PATH"
  private val RE_ANALYSIS_INTERVAL = "RE_ANALYSIS_INTERVAL"
  private val WORKER_POOL_SIZE = "WORKER_POOL_SIZE"
  private val REPEAT_DELAY = "REPEAT_DELAY"
  private val MAX_RETRIES = "MAX_RETRIES"

  object DefaultValues {
    val userAgent: String = "CoVerifiedBot-Extractor"
    val browseTimeout: Duration = Duration.ofMillis(60000L)
    val targetDateTimePattern = "yyyy-MM-dd'T'HH:mm:ssXXX"
    val targetTimeZone: ZoneId = ZoneId.of("UTC")
    val reAnalysisInterval: Duration = Duration.ofHours(48L)
    val workerPoolSize = 100
    val repeatDelay: Duration = Duration.ofSeconds(1L)
    val maxRetries: Int = 5
  }

  def apply(
      userAgent: String,
      browseTimeout: Duration,
      targetDateTimePattern: String,
      targetTimeZone: ZoneId,
      apiUrl: String,
      authSecret: String,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      workerPoolSize: Int,
      repeatDelay: Duration,
      maxRetries: Int
  ): Config =
    new Config(
      userAgent,
      browseTimeout,
      targetDateTimePattern,
      targetTimeZone,
      uri"$apiUrl",
      authSecret,
      profileDirectoryPath,
      reAnalysisInterval,
      workerPoolSize,
      repeatDelay,
      maxRetries
    )

  private val parseZoneIdOrDefault: String => ZoneId = (timeZoneId: String) =>
    Try(ZoneId.of(timeZoneId)) match {
      case Success(zoneId) => zoneId
      case Failure(_) =>
        logger.warn(
          "Given time zone identifier '{}' cannot be parsed to valid time zone. Take default one: '{}'",
          timeZoneId,
          DefaultValues.targetTimeZone
        )
        DefaultValues.targetTimeZone
    }

  /**
    * Build config from parsed CLI input
    *
    * @param args The parsed input
    * @return A try to create a [[Config]]
    */
  def fromArgs(args: Args): Try[Config] = args match {
    case Args(
        apiUrl,
        authSecret,
        pageProfileDirectoryPath,
        maybeReAnalysisInterval,
        maybeWorkerPoolSize,
        maybeRepeatDelay,
        maybeMaxRetries,
        maybeUserAgent,
        maybeBrowseTimeout,
        maybeTargetDateTimePattern,
        maybeTargetTimeZone
        ) =>
      Success(
        Config(
          maybeUserAgent.getOrElse(DefaultValues.userAgent),
          maybeBrowseTimeout
            .map(
              timeoutInSeconds => Duration.ofSeconds(timeoutInSeconds.toLong)
            )
            .getOrElse(DefaultValues.browseTimeout),
          maybeTargetDateTimePattern.getOrElse(
            DefaultValues.targetDateTimePattern
          ),
          maybeTargetTimeZone
            .map { timeZoneId =>
              parseZoneIdOrDefault(timeZoneId)
            }
            .getOrElse(DefaultValues.targetTimeZone),
          apiUrl,
          authSecret,
          pageProfileDirectoryPath,
          maybeReAnalysisInterval
            .map(
              reAnalysisInterval => Duration.ofHours(reAnalysisInterval.toLong)
            )
            .getOrElse(DefaultValues.reAnalysisInterval),
          maybeWorkerPoolSize.getOrElse(DefaultValues.workerPoolSize),
          maybeRepeatDelay
            .map(repeatDelay => Duration.ofSeconds(repeatDelay.toLong))
            .getOrElse(DefaultValues.repeatDelay),
          maybeMaxRetries.getOrElse(DefaultValues.maxRetries)
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
      userAgent <- fromEnv(USER_AGENT, identity, DefaultValues.userAgent)
      browseTimeout <- fromEnv(
        BROWSE_TIMEOUT,
        timeoutInSeconds => Duration.ofSeconds(timeoutInSeconds.toLong),
        DefaultValues.browseTimeout
      )
      targetDateTimePattern <- fromEnv(
        TARGET_DATE_TIME_PATTERN,
        identity,
        DefaultValues.targetDateTimePattern
      )
      targetTimeZone <- fromEnv(
        TARGET_TIME_ZONE,
        parseZoneIdOrDefault(_),
        DefaultValues.targetTimeZone
      )
      apiUrl <- fromEnv(API_URL, identity)
      profileDirectory <- fromEnv(PROFILE_DIRECTORY_PATH, identity)
      authSecret <- fromEnv(AUTH_SECRET, identity)
      reAnalysisInterval <- fromEnv(
        RE_ANALYSIS_INTERVAL,
        reAnalysisIntervalInHours =>
          Duration.ofHours(reAnalysisIntervalInHours.toLong),
        DefaultValues.reAnalysisInterval
      )
      workerPoolSize <- fromEnv(
        WORKER_POOL_SIZE,
        _.toInt,
        DefaultValues.workerPoolSize
      )
      repeatDelay <- fromEnv(
        REPEAT_DELAY,
        repeatDelayInSeconds => Duration.ofSeconds(repeatDelayInSeconds.toLong),
        DefaultValues.repeatDelay
      )
      maxRetries <- fromEnv(
        MAX_RETRIES,
        _.toInt,
        DefaultValues.maxRetries
      )
    } yield {
      Config(
        userAgent,
        browseTimeout,
        targetDateTimePattern,
        targetTimeZone,
        apiUrl,
        authSecret,
        profileDirectory,
        reAnalysisInterval,
        workerPoolSize,
        repeatDelay,
        maxRetries
      )
    }

  /**
    * Attempt to obtain an entry from environment variables
    *
    * @param key      The key to get
    * @param encoder  Transforming function
    * @return The value
    */
  private def fromEnv[T](key: String, encoder: String => T): Try[T] =
    (sys.env.get(key) match {
      case Some(value) => Success(value)
      case None =>
        Failure(
          ConfigException(s"Cannot find '$key' within environment variables.")
        )
    }).map(encoder(_))

  /**
    * Attempt to obtain an entry from environment variables. If the key cannot be obtained, take the default one.
    *
    * @param key      The key to get
    * @param encoder  Transforming function
    * @param default  The default value to recover with.
    * @return The value
    */
  private def fromEnv[T](
      key: String,
      encoder: String => T,
      default: T
  ): Try[T] =
    (sys.env.get(key) match {
      case Some(value) => Success(value)
      case None =>
        Failure(
          ConfigException(s"Cannot find '$key' within environment variables.")
        )
    }).map(encoder(_)).recover { _ =>
      logger.debug(
        "Environment variable '{}' not defined. Take default value '{}'.",
        key,
        if (!key.toLowerCase.contains("secret")) default
        else "***"
      )
      default
    }
}
