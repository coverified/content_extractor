/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  UpdateEntryInformation
}
import sttp.model.Uri

import java.time.{Duration, ZoneId}

sealed trait MutatorMessage
object MutatorMessage {

  final case class InitMutator(
      apiUri: Uri,
      authToken: String,
      reAnalysisInterval: Duration,
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage],
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage

  final case class CreateEntry(
      createEntryInformation: CreateEntryInformation,
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage
  final case class UpdateEntry(
      updateEntryInformation: UpdateEntryInformation,
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage

  final case class UpdateUrl(
      urlId: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends MutatorMessage

  final case object Terminate extends MutatorMessage

  final case class ConnectToTags(contentHash: Int, tagIds: Seq[String])
      extends MutatorMessage

}
