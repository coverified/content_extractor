/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.actor.UrlHandler.{
  handleUrlWithExistingEntry,
  handleUrlWithoutExistingEntry,
  visitUrlAndHandleEntry
}
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  RawEntryInformation
}
import info.coverified.extractor.messages.MutatorMessage.{
  CreateEntry,
  UpdateUrl
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  UrlHandledSuccessfully,
  UrlHandledWithFailure
}
import info.coverified.extractor.messages.{
  MutatorMessage,
  SourceHandlerMessage,
  UrlHandlerMessage
}
import info.coverified.extractor.messages.UrlHandlerMessage.{
  HandleExistingUrl,
  HandleNewUrl,
  InitUrlHandler
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.slf4j.Logger

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

        /* Visit the url and create an Entry right away, as no existing one needs consideration */
        val urlHandling = handleUrlWithoutExistingEntry(mutator, sourceHandler)
        visitUrlAndHandleEntry(
          analyzer,
          url,
          urlId,
          pageProfile,
          urlHandling,
          sourceHandler,
          context.log
        )

        /* Ask the mutator to update the url */
        mutator ! UpdateUrl(urlId, sourceHandler)
        Behaviors.same
      case (
          ctx,
          HandleExistingUrl(url, urlId, Some(entry), pageProfile, sourceHandler)
          ) =>
        ctx.log
          .debug(
            "Re-analyze already known url '{}' and respect existing entry with it '{}'.",
            url,
            entry.id
          )

        /* Visit the url and create an Entry under consideration of the existing one */
        val urlHandling =
          handleUrlWithExistingEntry(entry, mutator, sourceHandler)
        visitUrlAndHandleEntry(
          analyzer,
          url,
          urlId,
          pageProfile,
          urlHandling,
          sourceHandler,
          ctx.log
        )

        /* Ask the mutator to update the url */
        sourceHandler ! UrlHandledSuccessfully(url)
        Behaviors.same
      case (
          ctx,
          HandleExistingUrl(url, urlId, None, pageProfile, sourceHandler)
          ) =>
        ctx.log
          .debug(
            "Re-analyze already known url '{}'. No existing entry to consider.",
            url
          )

        /* Visit the url and create an Entry right away, as no existing one needs consideration */
        val urlHandling = handleUrlWithoutExistingEntry(mutator, sourceHandler)
        visitUrlAndHandleEntry(
          analyzer,
          url,
          urlId,
          pageProfile,
          urlHandling,
          sourceHandler,
          ctx.log
        )

        /* Ask the mutator to update the url */
        sourceHandler ! UrlHandledSuccessfully(url)
        Behaviors.same
      case _ => Behaviors.unhandled
    }
}

object UrlHandler {
  def apply(): Behavior[UrlHandlerMessage] = new UrlHandler().uninitialized

  type EntryHandlingFunction = (
      String,
      String,
      RawEntryInformation
  ) => Unit

  /**
    * Visit a given web page under the given url and create a new entry from content
    *
    * @param analyzer           Analyzer to use for page analysis
    * @param url                The url itself
    * @param urlId              Identifier of the url
    * @param pageProfile        Applicable page profile
    * @param handlingFunction   A function, that handles existing information as well as information from analyzer
    * @param sourceHandler      Reference to the source handler
    * @param logger             Logging instance
    */
  private def visitUrlAndHandleEntry(
      analyzer: Analyzer,
      url: String,
      urlId: String,
      pageProfile: ProfileConfig,
      handlingFunction: EntryHandlingFunction,
      sourceHandler: ActorRef[SourceHandlerMessage],
      logger: Logger
  ): Unit = analyzer.run(url, pageProfile) match {
    case Success(rawEntryInformation) =>
      logger.debug("Visiting of web site '{}' successful.", url)
      handlingFunction(
        url,
        urlId,
        rawEntryInformation
      )
    case Failure(exception) =>
      logger.error(
        "Error during visit of web site '{}'. Report to my source handler.",
        url
      )
      sourceHandler ! UrlHandledWithFailure(url, urlId, exception)
  }

  /**
    * A simple function, that handles an analyzed url, that does not have any kind of existing entry
    *
    * @param mutator            Reference to the mutator
    * @param sourceHandler      Reference to the source handler
    * @return A method to handle urls without given entry
    */
  private def handleUrlWithoutExistingEntry(
      mutator: ActorRef[MutatorMessage],
      sourceHandler: ActorRef[SourceHandlerMessage]
  ): EntryHandlingFunction =
    (
        url: String,
        urlId: String,
        rawEntryInformation: RawEntryInformation
    ) => {
      mutator ! CreateEntry(
        CreateEntryInformation(rawEntryInformation),
        urlId,
        sourceHandler
      )
      sourceHandler ! UrlHandledSuccessfully(url)
    }

  /**
    * A simple function, that handles an analyzed url, that does not have any kind of existing entry
    *
    * @param mutator            Reference to the mutator
    * @param sourceHandler      Reference to the source handler
    * @return A method to handle urls without given entry
    */
  private def handleUrlWithExistingEntry(
      existingEntry: SimpleEntryView[SimpleUrlView, String],
      mutator: ActorRef[MutatorMessage],
      sourceHandler: ActorRef[SourceHandlerMessage]
  ): EntryHandlingFunction =
    (
        url: String,
        urlId: String,
        rawEntryInformation: RawEntryInformation
    ) => {
      /* TODO:
       *  1) Check if content is equal
       *  2) Send update request or don't do anything
       */
      ???
    }
}
