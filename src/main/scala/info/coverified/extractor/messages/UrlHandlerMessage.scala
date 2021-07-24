/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef

trait UrlHandlerMessage
object UrlHandlerMessage {
  final case class HandleNewUrl(
      url: String,
      replyTo: ActorRef[SourceHandlerMessage]
  ) extends UrlHandlerMessage
}
