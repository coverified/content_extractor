/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import info.coverified.extractor.messages.MutatorMessage.{
  CreateEntry,
  UpdateUrl
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  UrlHandledSuccessfully,
  UrlHandledWithFailure
}
import info.coverified.extractor.messages.SupervisorMessage.ExistingUrlsHandled
import info.coverified.extractor.messages.{MutatorMessage, UrlHandlerMessage}
import info.coverified.extractor.messages.UrlHandlerMessage.{
  HandleExistingUrl,
  HandleNewUrl,
  InitUrlHandler
}

import scala.util.{Failure, Success}

/**
  * Extracting the content of a single url
  */
class UrlHandler {
  def uninitialized: Behaviors.Receive[UrlHandlerMessage] =
    Behaviors.receive[UrlHandlerMessage] {
      case (
          _,
          InitUrlHandler(
            mutatorRef,
            userAgent,
            browseTimeout,
            targetDateTimePattern,
            targetTimeZone
          )
          ) =>
        val analyzer = Analyzer(
          userAgent,
          browseTimeout,
          targetDateTimePattern,
          targetTimeZone
        )
        idle(mutatorRef, analyzer)
      case _ => Behaviors.unhandled
    }

  def idle(
      mutator: ActorRef[MutatorMessage],
      analyzer: Analyzer
  ): Behaviors.Receive[UrlHandlerMessage] =
    Behaviors.receive[UrlHandlerMessage] {
      case (context, HandleNewUrl(url, urlId, pageProfile, sourceHandler)) =>
        context.log.debug("Start content extraction for new url '{}'.", url)

        analyzer.run(url, pageProfile) match {
          case Success(rawEntryInformation) =>
            context.log.debug("Visiting of web site '{}' successful.", url)
            mutator ! CreateEntry(
              CreateEntryInformation(rawEntryInformation),
              urlId,
              sourceHandler
            )
            mutator ! UpdateUrl(urlId, sourceHandler)
            sourceHandler ! UrlHandledSuccessfully(url)
            Behaviors.same
          case Failure(exception) =>
            context.log.error(
              "Error during visit of web site '{}'. Report to my source handler.",
              url
            )
            mutator ! UpdateUrl(urlId, sourceHandler)
            sourceHandler ! UrlHandledWithFailure(url, urlId, exception)
            Behaviors.same
        }
      case (
          ctx,
          HandleExistingUrl(url, urlId, maybeEntry, pageProfile, sourceHandler)
          ) =>
        ctx.log
          .debug("Start content extraction for already visited url '{}'.", url)
        /* TODO: Insert logic */
        sourceHandler ! UrlHandledSuccessfully(url)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
}

object UrlHandler {
  def apply(): Behavior[UrlHandlerMessage] = new UrlHandler().uninitialized
}
