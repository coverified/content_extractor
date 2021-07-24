/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import sttp.model.Uri

sealed trait MutatorMessage
object MutatorMessage {
  final case class InitMutator(apiUri: Uri, authToken: String)
      extends MutatorMessage

  final case class CreateEntry(
      createEntryInformation: CreateEntryInformation,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage
  final case class UpdateUrl(
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage
}
