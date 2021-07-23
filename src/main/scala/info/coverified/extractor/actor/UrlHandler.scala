/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import info.coverified.extractor.actor.ExtractionSupervisor.{
  HandleNewUrls,
  NewUrlsHandled
}

object UrlHandler {
  def apply(): Behavior[HandleNewUrls] = Behaviors.receive[HandleNewUrls] {
    case (_, HandleNewUrls(data, replyTo)) =>
      replyTo ! new NewUrlsHandled()
      Behaviors.same
    case _ => Behaviors.unhandled
  }
}
