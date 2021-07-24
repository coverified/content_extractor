/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage
}
import info.coverified.extractor.messages.SourceHandlerMessage.InitSourceHandler
import info.coverified.extractor.messages.SupervisorMessage.SourceHandled
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import sttp.model.Uri

import java.time.Duration

/**
  * An actor, that handles the extraction process per source
  */
class SourceHandler {
  def uninitialized: Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (
          context,
          InitSourceHandler(
            apiUri,
            profileDirectoryPath,
            reAnalysisInterval,
            authSecret,
            chunkSize,
            repeatDelay,
            source,
            replyTo
          )
          ) =>
        context.log.info(
          "Initializing a source handler for source '{}' (to be found at '{}').",
          source.id,
          source.url
        )
        val stateData = SourceHandlerStateData(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay,
          source,
          replyTo
        )
        // TODO: Do something
        replyTo ! SourceHandled(source.id)
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  final case class SourceHandlerStateData(
      apiUri: Uri,
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      supervisor: ActorRef[SupervisorMessage]
  )
}

object SourceHandler {
  def apply(): Behavior[SourceHandlerMessage] =
    new SourceHandler().uninitialized
}
