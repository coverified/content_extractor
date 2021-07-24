/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.messages.SourceHandlerMessage.{
  NewUrlHandledSuccessfully,
  NewUrlHandledWithFailure
}
import info.coverified.extractor.messages.UrlHandlerMessage
import info.coverified.extractor.messages.UrlHandlerMessage.HandleNewUrl
import scala.util.{Failure, Success}

/**
  * Extracting the content of a single url
  */
class UrlHandler {
  def idle: Behaviors.Receive[UrlHandlerMessage] =
    Behaviors.receive[UrlHandlerMessage] {
      case (context, HandleNewUrl(url, pageProfile, replyTo)) =>
        context.log.info("Start content extraction for new url '{}'.", url)

        Analyzer.run(url, pageProfile) match {
          case Success(rawEntryInformation) =>
            context.log.debug("Visiting of web site '{}' successful.", url)
            replyTo ! NewUrlHandledSuccessfully(url)
            Behaviors.same
          case Failure(exception) =>
            context.log.error(
              "Error during visit of web site '{}'. Report to my source handler.",
              url
            )
            replyTo ! NewUrlHandledWithFailure(url, exception)
            Behaviors.same
        }
      case _ => Behaviors.unhandled
    }
}

object UrlHandler {
  def apply(): Behavior[UrlHandlerMessage] = new UrlHandler().idle
}
