/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
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
      profileDirectoryPath: String,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      replyTo: ActorRef[SupervisorMessage]
  ) extends SourceHandlerMessage

  final case class Run(replyTo: ActorRef[SupervisorMessage])
      extends SourceHandlerMessage
}
