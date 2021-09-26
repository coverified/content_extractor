/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.actor.UrlHandler.{
  failIfContentUnchanged,
  handleUrlWithExistingEntry,
  handleUrlWithoutExistingEntry,
  sendSuccess,
  visitUrlAndHandleEntry
}
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.analyzer.Analyzer.ContentUnchanged
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  RawEntryInformation,
  UpdateEntryInformation
}
import info.coverified.extractor.exceptions.AnalysisException
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
import info.coverified.graphql.GraphQLHelper
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
            targetTimeZone,
            apiUri,
            authSecret
          )
          ) =>
        val analyzer = Analyzer(
          userAgent,
          browseTimeout,
          targetDateTimePattern,
          targetTimeZone
        )
        val graphQLHelper = new GraphQLHelper(apiUri, authSecret)
        idle(mutatorRef, analyzer, graphQLHelper)
      case _ => Behaviors.unhandled
    }

  def idle(
      mutator: ActorRef[MutatorMessage],
      analyzer: Analyzer,
      graphQLHelper: GraphQLHelper
  ): Behaviors.Receive[UrlHandlerMessage] =
    Behaviors.receive[UrlHandlerMessage] {
      case (context, HandleNewUrl(url, urlId, pageProfile, sourceHandler)) =>
        context.log.debug("Start content extraction for new url '{}'.", url)

        /* Visit the url and create an Entry right away, as no existing one needs consideration */
        val handleUnchanged = failIfContentUnchanged(sourceHandler)
        val handleChanged =
          handleUrlWithoutExistingEntry(mutator, sourceHandler)
        visitUrlAndHandleEntry(
          analyzer = analyzer,
          url = url,
          urlId = urlId,
          maybeETag = None,
          payLoad = None,
          pageProfile = pageProfile,
          handleUnchanged = handleUnchanged,
          handleChanged = handleChanged,
          sourceHandler = sourceHandler,
          logger = context.log
        )

        /* Ask the mutator to update the url */
        mutator ! UpdateUrl(urlId, sourceHandler)
        Behaviors.same
      case (
          ctx,
          HandleExistingUrl(url, urlId, maybeEntry, pageProfile, sourceHandler)
          ) =>
        /* Determine the url handling based on the provided entry information. If nothing has been delivered, yet, it
         * might be, because the url handler is triggered the first time for this url or the analysis of this url is
         * re-scheduled and there actually is no entry. Anyway, try to receive it, as there is no chance for
         * distinction, yet. */
        val (handleUnchanged, handleChanged) = maybeEntry
          .orElse {
            ctx.log.debug(
              "No entry information delivered for url '{}' ('{}'). Try to get matching entry from API.",
              url,
              urlId
            )
            graphQLHelper.queryMatchingEntry(urlId)
          }
          .map { entry =>
            ctx.log
              .debug(
                "Re-analyze already known url '{}' and respect existing entry with id '{}'.",
                url,
                entry.id
              )
            (
              sendSuccess(sourceHandler),
              handleUrlWithExistingEntry(entry, mutator, sourceHandler)
            )
          }
          .getOrElse {
            ctx.log
              .debug(
                "Re-analyze already known url '{}'. No existing entry to consider.",
                url
              )
            (
              failIfContentUnchanged(sourceHandler),
              handleUrlWithoutExistingEntry(mutator, sourceHandler)
            )
          }

        /* Visit the url and create or update an Entry */
        visitUrlAndHandleEntry(
          analyzer = analyzer,
          url = url,
          urlId = urlId,
          maybeETag = maybeEntry.flatMap(_.eTag),
          payLoad = maybeEntry,
          pageProfile = pageProfile,
          handleUnchanged = handleUnchanged,
          handleChanged = handleChanged,
          sourceHandler = sourceHandler,
          logger = ctx.log
        )

        /* Ask the mutator to update the url */
        mutator ! UpdateUrl(urlId, sourceHandler)
        Behaviors.same

      case (ctx, UrlHandlerMessage.Terminate) =>
        ctx.log.debug("Url handler shutting down")
        graphQLHelper.close()
        Behaviors.stopped { () =>
          ctx.log.debug("Url Handler shut down.")
        }

      case _ => Behaviors.unhandled
    }
}

object UrlHandler {
  def apply(): Behavior[UrlHandlerMessage] = new UrlHandler().uninitialized

  type HandleActualInformationFunction = (
      String, // The actual url
      String, // The url id
      RawEntryInformation // raw web information
  ) => Unit
  type HandleUnchangedInformationFunction = (
      String, // The actual url
      String // The url id
  ) => Unit

  /**
    * Visit a given web page under the given url and create a new entry from content
    *
    * @param analyzer         Analyzer to use for page analysis
    * @param url              The url itself
    * @param urlId            Identifier of the url
    * @param maybeETag        Optional HTTP Entity tag to possibly detect unchanged content
    * @param payLoad          Possible pay load of request
    * @param pageProfile      Applicable page profile
    * @param handleUnchanged  A function, that handles existing information in the case, that web page's information
    *                         hasn't changed
    * @param handleChanged    A function, that handles existing information as well as actual information from analyzer
    * @param sourceHandler    Reference to the source handler
    * @param logger           Logging instance
    */
  private def visitUrlAndHandleEntry[P](
      analyzer: Analyzer,
      url: String,
      urlId: String,
      maybeETag: Option[String],
      payLoad: Option[P],
      pageProfile: ProfileConfig,
      handleUnchanged: HandleUnchangedInformationFunction,
      handleChanged: HandleActualInformationFunction,
      sourceHandler: ActorRef[SourceHandlerMessage],
      logger: Logger
  ): Unit = analyzer.run(url, pageProfile, maybeETag) match {
    case Success(Left(ContentUnchanged)) =>
      logger.debug(
        "Visiting of web site '{}' successful. Content hasn't changed. Handle it!",
        url
      )
      handleUnchanged(url, urlId)
    case Success(Right(rawEntryInformation)) =>
      logger.debug(
        "Visiting of web site '{}' successful. Received actual content. Handle it!",
        url
      )
      handleChanged(
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
    * Sends out a [[UrlHandledWithFailure]] message, as there shouldn't be the chance to compare the content against
    * anything else.
    *
    * @param sourceHandler Reference to the source handler
    * @return A function, that does exactly that
    */
  private def failIfContentUnchanged(
      sourceHandler: ActorRef[SourceHandlerMessage]
  ): HandleUnchangedInformationFunction =
    (
        url: String,
        urlId: String
    ) => {
      sourceHandler ! UrlHandledWithFailure(
        url,
        urlId,
        None,
        AnalysisException(
          s"Content of '$url' ($urlId) detected as unchanged although I don't have an ETag at hand to compare against..."
        )
      )
    }

  /**
    * Send out success report to source handler, as no information have changed
    *
    * @param sourceHandler Reference to the source handler
    * @return A function, that does exactly that
    */
  private def sendSuccess(
      sourceHandler: ActorRef[SourceHandlerMessage]
  ): HandleUnchangedInformationFunction =
    (
        url: String,
        _: String
    ) => {
      sourceHandler ! UrlHandledSuccessfully(url)
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
  ): HandleActualInformationFunction =
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
  ): HandleActualInformationFunction =
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
          _,
          _
        ),
        RawEntryInformation(
          scrapedTitle,
          scrapedSummary,
          scrapedContent,
          scrapedDate,
          _,
          _,
          _
        )
        ) =>
      !(maybeTitle
        .contains(scrapedTitle) && maybeSummary == scrapedSummary && maybeContent == scrapedContent && maybeDate == scrapedDate)
  }
}
