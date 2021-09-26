/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import sttp.model.Uri

sealed trait DistinctTagHandlerMessage
object DistinctTagHandlerMessage {
  final case class InitializeDistinctTagHandler(
      apiUri: Uri,
      authSecret: String,
      replyTo: ActorRef[SupervisorMessage]
  ) extends DistinctTagHandlerMessage
  final case class ConsolidateArticleTags(
      contentHash: Int,
      tags: List[String],
      replyTo: ActorRef[MutatorMessage]
  ) extends DistinctTagHandlerMessage
  case object Terminate extends DistinctTagHandlerMessage
}
