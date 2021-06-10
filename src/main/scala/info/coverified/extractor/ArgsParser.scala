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
      apiUrl: Option[String] = None,
      pageProfileFolderPath: Option[String] = None,
      reAnalyzeInterval: Option[Int] = None,
      authSecret: Option[String] = None
  )

  private def buildParser: scoptOptionParser[Args] = {
    new scoptOptionParser[Args]("CoVerifiedExtractor") {
      opt[String]("apiUrl")
        .action((value, args) => {
          args.copy(
            apiUrl = Option(value)
          )
        })
        .validate(
          value =>
            if (value.trim.isEmpty) failure("apiUrl cannot be empty!")
            else success
        )
        .text("Backend API Url")
        .minOccurs(1)
      opt[String]("pageProfileFolderPath")
        .action((value, args) => {
          args.copy(
            pageProfileFolderPath = Option(value)
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
      opt[Int]("reAnalyzeInterval")
        .action((value, args) => {
          args.copy(
            reAnalyzeInterval = Option(value)
          )
        })
        .validate(
          value =>
            if (value < 0)
              failure("re analysis interval must be greater than zero!")
            else success
        )
        .text("frequency of re-analyzing content in hours")
        .minOccurs(1)
      opt[String]("authSecret")
        .action((value, args) => {
          args.copy(
            authSecret = Option(value)
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
    }

  }

  def parse(args: Array[String]): Option[Args] =
    buildParser.parse(args, init = Args())

}
