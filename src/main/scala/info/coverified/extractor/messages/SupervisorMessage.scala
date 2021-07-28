/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.config.Config
import sttp.model.Uri

import java.time.{Duration, ZoneId}

/**
  * All messages that are understood by the [[info.coverified.extractor.actor.ExtractionSupervisor]]
  */
sealed trait SupervisorMessage
object SupervisorMessage {
  final case class InitSupervisor(
      userAgent: String,
      browseTimeout: Duration,
      targetDateTimePattern: String,
      targetTimeZone: ZoneId,
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      maxRetries: Int,
      authSecret: String,
      workerPoolSize: Int,
      repeatDelay: Duration
  ) extends SupervisorMessage
  object InitSupervisor {
    def apply(config: Config): InitSupervisor = config match {
      case Config(
          userAgent,
          browseTimeout,
          targetDateTimePattern,
          targetTimeZone,
          apiUri,
          authSecret,
          profileDirectoryPath,
          reAnalysisInterval,
          workerPoolSize,
          repeatDelay,
          maxRetries
          ) =>
        new InitSupervisor(
          userAgent,
          browseTimeout,
          targetDateTimePattern,
          targetTimeZone,
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          maxRetries,
          authSecret,
          workerPoolSize,
          repeatDelay
        )
    }
  }

  case object DistinctTagHandlerInitialized extends SupervisorMessage
  final case class SourceHandlerInitialized(
      sourceId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends SupervisorMessage

  /**
    * Report back, that the new urls have been handled
    *
    * @param sourceId Identifier of the source
    */
  final case class NewUrlsHandled(
      sourceId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends SupervisorMessage

  /**
    * Report back, that the existing urls have been handled
    *
    * @param sourceId Identifier of the source
    */
  final case class ExistingUrlsHandled(
      sourceId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends SupervisorMessage

  final case class SourceHandlerTerminated(sourceId: String)
      extends SupervisorMessage

  object DistinctTagHandlerTerminated extends SupervisorMessage
}
