/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors._
import info.coverified.extractor.messages.SourceHandlerMessage.InitSourceHandler
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage
}
import info.coverified.extractor.messages.SupervisorMessage.{
  InitSupervisor,
  SourceHandled
}
import info.coverified.graphql.GraphQLHelper
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
        msg @ InitSupervisor(
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

      /* Query all sources */
      new GraphQLHelper(stateData.apiUri, stateData.authSecret).queryAllSources match {
        case Some(emptySources) if emptySources.isEmpty =>
          context.log.info("There are no sources available. I'm done!")
          Behaviors.stopped
        case Some(sources) =>
          context.log.info(
            "Received {} sources. Spawn an actor for each of them.",
            sources.size
          )

          val activeSources = sources.map { source =>
            context.log.debug(
              "Spawning an actor for source '{}' ('{}').",
              source.id,
              source.url
            )
            val handler =
              context.spawn(SourceHandler(), "SourceHandler_" + source.id)
            handler ! InitSourceHandler(
              apiUri,
              profileDirectoryPath,
              reAnalysisInterval,
              authSecret,
              chunkSize,
              repeatDelay,
              source,
              context.self
            )
            source.id -> handler
          }.toMap
          handleSourceResponses(stateData, activeSources)
        case None =>
          context.log.warn(
            "Querying sources did not return a sensible reply. Shut down."
          )
          Behaviors.stopped
      }
    case _ => unhandled
  }

  /**
    * Handle the responses from source handlers
    *
    * @param stateData State information
    * @return The specified behavior
    */
  def handleSourceResponses(
      stateData: ExtractorStateData,
      activeSources: Map[String, ActorRef[SourceHandlerMessage]]
  ): Receive[SupervisorMessage] = Behaviors.receive[SupervisorMessage] {
    case (context, SourceHandled(sourceId)) =>
      context.log
        .debug("Handler for source '{}' reported to have finished.", sourceId)
      val stillActiveSources = activeSources.filterNot {
        case (key, _) => key == sourceId
      }
      if (stillActiveSources.nonEmpty) {
        context.log.debug(
          "Still waiting for the following sources to terminate:\n\t{}",
          stillActiveSources.mkString("\n\t")
        )
        handleSourceResponses(stateData, stillActiveSources)
      } else {
        context.log.info(
          "All sources have reported to have finished. Good night! zzz"
        )
        Behaviors.stopped
      }
    case _ => Behaviors.unhandled
  }

  final case class ExtractorStateData(
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration
  )
}
