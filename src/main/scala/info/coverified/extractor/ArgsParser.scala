package info.coverified.extractor

import scopt.{OptionParser => scoptOptionParser}

/**
 * //ToDo: Class Description
 *
 * @version 0.1
 * @since 27.02.21
 */
object ArgsParser {

  final case class Args(
                         apiUrl: Option[String] = None,
                         pageProfileFolderPath: Option[String] = None
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

    }

  }

  def parse(args: Array[String]): Option[Args] =
    buildParser.parse(args, init = Args())

}
