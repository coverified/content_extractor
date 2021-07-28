/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import scopt.{OptionParser => scoptOptionParser}

/**
  * Parsing needed extractor configuration from CLI arguments
  *
  * @version 0.1
  * @since 27.02.21
  */
object ArgsParser {

  final case class Args(
      apiUrl: String,
      authSecret: String,
      pageProfileDirectoryPath: String,
      reAnalysisInterval: Option[Int] = None,
      workerPoolSize: Option[Int] = None,
      repeatDelay: Option[Int] = None,
      maxRetries: Option[Int] = None,
      userAgent: Option[String] = None,
      browseTimeout: Option[Int] = None,
      targetDateTimePattern: Option[String] = None,
      targetTimeZone: Option[String] = None
  )

  private def buildParser: scoptOptionParser[Args] = {
    new scoptOptionParser[Args]("CoVerifiedExtractor") {
      opt[String]("apiUrl")
        .required()
        .action { (value, args) =>
          {
            args.copy(
              apiUrl = value
            )
          }
        }
        .validate(
          value =>
            if (value.trim.isEmpty) failure("apiUrl cannot be empty!")
            else success
        )
        .text("Backend API Url")
        .minOccurs(1)
      opt[String]("authSecret")
        .required()
        .action((value, args) => {
          args.copy(
            authSecret = value
          )
        })
        .validate(
          value =>
            if (value.trim.isEmpty)
              failure("auth secret cannot be empty!")
            else success
        )
        .text("secret to authenticate against API")
        .minOccurs(1)
      opt[String]("pageProfileDirectoryPath")
        .required()
        .action((value, args) => {
          args.copy(
            pageProfileDirectoryPath = value
          )
        })
        .validate(
          value =>
            if (value.trim.isEmpty)
              failure("page profile folder path cannot be empty!")
            else success
        )
        .text("full path to all page profile config files")
        .minOccurs(1)
      opt[Int]("reAnalysisInterval")
        .action((value, args) => {
          args.copy(
            reAnalysisInterval = Option(value)
          )
        })
        .validate(
          value =>
            if (value < 0)
              failure("re analysis interval must be greater than zero!")
            else success
        )
        .text("frequency of re-analyzing content in hours")
      opt[Int]("workerPoolSize")
        .action((value, args) => {
          args.copy(
            workerPoolSize = Option(value)
          )
        })
        .validate(
          value =>
            if (value <= 0)
              failure("Amount of workers may be greater than zero!")
            else success
        )
        .text("Amount of url workers for parallel handling.")
      opt[Int]("repeatDelay")
        .action((value, args) => {
          args.copy(
            repeatDelay = Option(value)
          )
        })
        .validate(
          value =>
            if (value <= 0)
              failure("Repeat delay may be greater than zero!")
            else success
        )
        .text(
          "Amount of seconds, that successive runs should be delayed, if not all urls are handled, yet."
        )
      opt[Int]("maxRetries")
        .action((value, args) => {
          args.copy(
            maxRetries = Option(value)
          )
        })
        .validate(
          value =>
            if (value <= 0)
              failure("Amount of retries may be greater than zero!")
            else success
        )
        .text(
          "Amount of retries, if a website reports rate limit exceeding."
        )
      opt[String]("userAgent")
        .action { (value, args) =>
          {
            args.copy(userAgent = Option(value))
          }
        }
        .text(
          "User agent information, that needs to be send when visiting websites"
        )
      opt[Int]("browseTimeout")
        .action { (value, args) =>
          {
            args.copy(browseTimeout = Option(value))
          }
        }
        .validate { timeout =>
          if (timeout < 0) failure("Browse timeout may be greater than zero.")
          else success
        }
        .text(
          "Timeout in seconds to apply, when visiting a website."
        )
      opt[String]("targetDateTimePattern")
        .action { (value, args) =>
          {
            args.copy(targetDateTimePattern = Option(value))
          }
        }
        .text(
          "Target pattern, when sending date time information to GraphQL API."
        )
      opt[String]("targetTimeZone")
        .action { (value, args) =>
          {
            args.copy(targetTimeZone = Option(value))
          }
        }
        .text(
          "Target time zone, when sending date time information to GraphQL API."
        )
    }

  }

  def parse(args: Array[String]): Option[Args] =
    buildParser.parse(args, init = Args("", "", ""))
}
