/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import sttp.model.Uri

import java.time.Duration

/**
  * All messages that are understood by the [[info.coverified.extractor.actor.SourceHandler]]
  */
sealed trait SourceHandlerMessage
object SourceHandlerMessage {
  final case class InitSourceHandler(
      apiUri: Uri,
      pageProfile: ProfileConfig,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage],
      replyTo: ActorRef[SupervisorMessage]
  ) extends SourceHandlerMessage

  object MutatorInitialized extends SourceHandlerMessage

  final case class Run(replyTo: ActorRef[SupervisorMessage])
      extends SourceHandlerMessage

  sealed trait NewUrlHandledMessage extends SourceHandlerMessage
  final case class NewUrlHandledSuccessfully(url: String)
      extends NewUrlHandledMessage
  final case class NewUrlHandledWithFailure(
      url: String,
      urlId: String,
      failure: Throwable
  ) extends NewUrlHandledMessage

  final case class ReScheduleUrl(url: String, id: String)
      extends SourceHandlerMessage

  final case object MutationsCompleted extends SourceHandlerMessage
}
