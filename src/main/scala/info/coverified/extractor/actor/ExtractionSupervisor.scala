/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors._
import info.coverified.extractor.config.Config
import sttp.model.Uri

import java.time.Duration

object ExtractionSupervisor {
  def apply(): Behavior[SupervisorMessage] = uninitialized

  /**
    * Start uninitialized
    */
  def uninitialized: Receive[SupervisorMessage] = receive[SupervisorMessage] {
    case (
        context,
        msg @ InitSupervisorMessage(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
        )
        ) =>
      context.log.info("Received a init message: {}.", msg)
      /* Set up state data */
      val stateData = ExtractorStateData(
        apiUri,
        profileDirectoryPath,
        reAnalysisInterval,
        authSecret,
        chunkSize,
        repeatDelay
      )

      /* Spawn an url handler and ask it to handle all new urls */
      context.log.info("Attempt to handle new urls")
      val urlHandler = context.spawn(UrlHandler(), "UrlHandler")
      urlHandler ! HandleNewUrls(stateData, context.self)
      handlingNewUrls(stateData)
    case _ => unhandled
  }

  /**
    * Handle all events in the process of handling new urls
    *
    * @param data state data of the extractor
    */
  def handlingNewUrls(data: ExtractorStateData): Receive[SupervisorMessage] =
    receive[SupervisorMessage] {
      case (context, _: NewUrlsHandled) =>
        context.log.info("All new urls are handled. Stop myself.")
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  private final case class ExtractorStateData(
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration
  )

  /* ===== Message definition ===== */
  sealed trait SupervisorMessage
  final case class InitSupervisorMessage(
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration
  ) extends SupervisorMessage
  object InitSupervisorMessage {
    def apply(config: Config): InitSupervisorMessage = config match {
      case Config(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
          ) =>
        new InitSupervisorMessage(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
        )
    }
  }
  final case class HandleNewUrls(
      data: ExtractorStateData,
      replyTo: ActorRef[NewUrlsHandled]
  ) extends SupervisorMessage
  class NewUrlsHandled() extends SupervisorMessage
}
