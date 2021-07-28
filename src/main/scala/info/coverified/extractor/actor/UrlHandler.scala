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
  RawEntryInformation,
  UpdateEntryInformation
}
import info.coverified.extractor.messages.MutatorMessage.{
  CreateEntry,
  UpdateEntry,
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
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
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
          None,
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
          HandleExistingUrl(
            url,
            urlId,
            payload @ Some(entry),
            pageProfile,
            sourceHandler
          )
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
          payload,
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
          None,
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
    * @param payLoad            Possible pay load of request
    * @param pageProfile        Applicable page profile
    * @param handlingFunction   A function, that handles existing information as well as information from analyzer
    * @param sourceHandler      Reference to the source handler
    * @param logger             Logging instance
    */
  private def visitUrlAndHandleEntry[P](
      analyzer: Analyzer,
      url: String,
      urlId: String,
      payLoad: Option[P],
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
      sourceHandler ! UrlHandledWithFailure(url, urlId, payLoad, exception)
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
      existingEntry: SimpleEntryView[SimpleUrlView, ArticleTagView],
      mutator: ActorRef[MutatorMessage],
      sourceHandler: ActorRef[SourceHandlerMessage]
  ): EntryHandlingFunction =
    (
        url: String,
        urlId: String,
        rawEntryInformation: RawEntryInformation
    ) => {
      /* Check, if anything has changed */
      val tagsChanged =
        tagsHaveChanged(existingEntry.articleTags, rawEntryInformation.tags)
      val contentChanged = contentHasChanged(existingEntry, rawEntryInformation)

      /* If content has changed, update it. If nothing has changed, don't do anything and report finished handling. */
      if (tagsChanged || contentChanged) {
        mutator ! UpdateEntry(
          UpdateEntryInformation(existingEntry.id, rawEntryInformation),
          urlId,
          sourceHandler
        )
      }

      sourceHandler ! UrlHandledSuccessfully(url)
    }

  /**
    * Figure out, if something regarding the page provided tags has changed
    *
    * @param maybeExistingTags Collection of tags registered in existing entry
    * @param maybePageTags     Tags from web page
    * @return true, if something has changed
    */
  private def tagsHaveChanged(
      maybeExistingTags: Option[List[ArticleTagView]],
      maybePageTags: Option[List[String]]
  ): Boolean =
    maybeExistingTags
      .map { existingPageTags =>
        val sameAmount = existingPageTags.size != maybePageTags
          .map(_.size)
          .getOrElse(0)
        val nothingChanged = maybePageTags
          .map(
            tags =>
              !tags.forall(
                tag => existingPageTags.exists(_.name.contains(tag))
              )
          )
          .getOrElse {
            /* Tags from web page are empty. Something has changed, if there are page tags apparent in the known entry. */
            existingPageTags.nonEmpty
          }
        sameAmount && nothingChanged
      }
      .getOrElse {
        /* Tags in existing entry are empty. There has something changed, if the page tags aren't empty */
        maybePageTags.nonEmpty && maybePageTags.exists(_.nonEmpty)
      }

  /**
    * Check, if the content of given entry and newly scraped information differs
    *
    * @param existingEntry        The existing entry
    * @param rawEntryInformation  Freshly scraped information
    * @return True, if something has changed
    */
  private def contentHasChanged(
      existingEntry: SimpleEntryView[SimpleUrlView, ArticleTagView],
      rawEntryInformation: RawEntryInformation
  ): Boolean = (existingEntry, rawEntryInformation) match {
    case (
        SimpleEntryView(
          _,
          maybeTitle,
          maybeContent,
          maybeSummary,
          _,
          maybeDate,
          _,
          _
        ),
        RawEntryInformation(
          scrapedTitle,
          scrapedSummary,
          scrapedContent,
          scrapedDate,
          _
        )
        ) =>
      !(maybeTitle
        .contains(scrapedTitle) && maybeSummary == scrapedSummary && maybeContent == scrapedContent && maybeDate == scrapedDate)
  }
}
