/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import info.coverified.extractor.config.Config
import sttp.model.Uri

import java.time.Duration

/**
  * All messages that are understood by the [[info.coverified.extractor.actor.ExtractionSupervisor]]
  */
sealed trait SupervisorMessage
object SupervisorMessage {
  final case class InitSupervisor(
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration
  ) extends SupervisorMessage
  object InitSupervisor {
    def apply(config: Config): InitSupervisor = config match {
      case Config(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
          ) =>
        new InitSupervisor(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
        )
    }
  }

  /**
    * Message indicates, that a certain source has been treated
    *
    * @param sourceId Identifier of the source
    */
  final case class SourceHandled(sourceId: String) extends SupervisorMessage
}
