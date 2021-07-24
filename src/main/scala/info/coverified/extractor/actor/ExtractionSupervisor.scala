/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors._
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
  Run
}
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage
}
import info.coverified.extractor.messages.SupervisorMessage.{
  InitSupervisor,
  SourceHandled,
  SourceHandlerInitialized
}
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
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

          val initializedSources = sources.map { source =>
            context.log.debug(
              "Spawning an actor for source '{}' ('{}').",
              source.id,
              source.name.getOrElse("")
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
          handleSourceResponses(stateData, initializedSources, Map.empty)
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
    * @param stateData          State information
    * @param initializedSources Mapping of all yet initialized sources
    * @param activeSources      Mapping of all sources, that have been initialized
    * @return The specified behavior
    */
  def handleSourceResponses(
      stateData: ExtractorStateData,
      initializedSources: Map[String, ActorRef[SourceHandlerMessage]],
      activeSources: Map[String, ActorRef[SourceHandlerMessage]]
  ): Receive[SupervisorMessage] = Behaviors.receive[SupervisorMessage] {
    case (context, SourceHandlerInitialized(sourceId, replyTo)) =>
      context.log.debug(
        "Source handler for source '{}' successfully initialized. Trigger it to start action!",
        sourceId
      )
      val stillToBeActivedSourceHandlers = initializedSources.filterNot {
        case (key, _) => key == sourceId
      }
      if (stillToBeActivedSourceHandlers.nonEmpty)
        context.log.debug(
          "Still initializing source handler for sources:\n\t{}",
          stillToBeActivedSourceHandlers.mkString("\n\t")
        )
      else
        context.log.debug("All source handlers initialized.")
      val newActiveSource = activeSources + (sourceId -> replyTo)
      replyTo ! Run(context.self)
      handleSourceResponses(
        stateData,
        stillToBeActivedSourceHandlers,
        newActiveSource
      )
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
        handleSourceResponses(stateData, initializedSources, stillActiveSources)
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
