/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.profile.ProfileConfig

sealed trait UrlHandlerMessage
object UrlHandlerMessage {
  final case class InitUrlHandler(mutator: ActorRef[MutatorMessage])
      extends UrlHandlerMessage
  final case class HandleNewUrl(
      url: String,
      urlId: String,
      pageProfile: ProfileConfig,
      replyToSourceHandler: ActorRef[SourceHandlerMessage]
  ) extends UrlHandlerMessage
}
