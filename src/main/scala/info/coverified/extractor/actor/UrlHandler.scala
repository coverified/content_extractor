/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.SourceHandlerMessage.NewUrlHandled
import info.coverified.extractor.messages.UrlHandlerMessage
import info.coverified.extractor.messages.UrlHandlerMessage.HandleNewUrl

class UrlHandler {
  def idle: Behaviors.Receive[UrlHandlerMessage] =
    Behaviors.receive[UrlHandlerMessage] {
      case (context, HandleNewUrl(url, replyTo)) =>
        context.log.info("I got asked to handle a new url '{}'.", url)
        replyTo ! NewUrlHandled(url)
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }
}
object UrlHandler {
  def apply(): Behavior[UrlHandlerMessage] = new UrlHandler().idle
}
