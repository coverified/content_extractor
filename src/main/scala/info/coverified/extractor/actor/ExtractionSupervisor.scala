/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors._
import info.coverified.extractor.messages.SupervisorMessage
import info.coverified.extractor.messages.SupervisorMessage.{
  InitSupervisorMessage,
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

          val activeSources = sources.map {
            source =>
              /* TODO: Spawn a source handler and ask it to handle do it's stuff */
              // context.log.info("Attempt to handle new urls")
              // val urlHandler = context.spawn(UrlHandler(), "UrlHandler")
              // urlHandler ! HandleNewUrls(stateData, context.self)
              // handlingNewUrls(stateData)
              source.id
          }
          handleSourceResponses(stateData, activeSources.toVector)
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
      activeSources: Vector[String]
  ): Receive[SupervisorMessage] = Behaviors.receive[SupervisorMessage] {
    case (context, SourceHandled(sourceId)) =>
      context.log.debug("Handler for source '{}' reported to have finished.")
      val stillActiveSources = activeSources.filterNot(_ == sourceId)
      if (stillActiveSources.nonEmpty) {
        context.log.debug(
          "Still waiting for the following sources to terminate:\n\t{}",
          stillActiveSources.mkString("\n\t")
        )
        Behaviors.same
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
