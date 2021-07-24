/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import sttp.model.Uri

import java.time.Duration

sealed trait MutatorMessage
object MutatorMessage {

  final case class InitMutator(
      apiUri: Uri,
      authToken: String,
      reAnalysisInterval: Duration
  ) extends MutatorMessage

  final case class CreateEntry(
      createEntryInformation: CreateEntryInformation,
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage

  final case class UpdateUrl(
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage

}
