/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import info.coverified.extractor.actor.UrlHandlingSupport.{
  SimpleUrl,
  UrlQueueObject,
  UrlWithPayLoad
}
import info.coverified.extractor.actor.SourceHandler.{
  SourceHandlerStateData,
  peek
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  ScheduleUrl,
  UrlHandledSuccessfully,
  UrlHandledWithFailure
}
import info.coverified.extractor.messages.SupervisorMessage.{
  ExistingUrlsHandled,
  NewUrlsHandled
}
import info.coverified.extractor.messages.UrlHandlerMessage.HandleNewUrl
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage,
  UrlHandlerMessage
}
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.jsoup.HttpStatusException
import org.slf4j.Logger

import java.time.Duration
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

trait UrlHandlingSupport {
  this: SourceHandler =>

  /**
    * Behavior to steer the handling of new urls
    *
    * @param stateData       Current state of the actor
    * @param urlsToBeHandled List of urls, that need to be addressed
    * @param urlToActivation Mapping from activated url to it's activation time
    * @param workerPoolProxy Reference to the worker pool proxy
    * @return The defined behavior
    */
  def handleNewUrls(
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[SimpleUrl],
      urlToActivation: Map[String, Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      timer: TimerScheduler[SourceHandlerMessage],
      context: ActorContext[SourceHandlerMessage]
  ): Behaviors.Receive[SourceHandlerMessage] = handleUrls(
    stateData,
    urlsToBeHandled,
    urlToActivation,
    workerPoolProxy,
    timer,
    NewUrlsHandled(stateData.source.id, context.self),
    (
        remainingUrls: List[SimpleUrl],
        updatedUrlToActivation: Map[String, Long]
    ) =>
      handleNewUrls(
        stateData,
        remainingUrls,
        updatedUrlToActivation,
        workerPoolProxy,
        timer,
        context
      )
  )

  /**
    * Behavior to steer the handling of new urls
    *
    * @param stateData       Current state of the actor
    * @param urlsToBeHandled List of urls, that need to be addressed
    * @param urlToActivation Mapping from activated url to it's activation time
    * @param workerPoolProxy Reference to the worker pool proxy
    * @return The defined behavior
    */
  def handleExistingUrls(
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[
        UrlWithPayLoad[SimpleEntryView[SimpleUrlView, ArticleTagView]]
      ],
      urlToActivation: Map[String, Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      timer: TimerScheduler[SourceHandlerMessage],
      context: ActorContext[SourceHandlerMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    handleUrls[UrlWithPayLoad[SimpleEntryView[SimpleUrlView, ArticleTagView]], SimpleEntryView[
      SimpleUrlView,
      ArticleTagView
    ]](
      stateData,
      urlsToBeHandled,
      urlToActivation,
      workerPoolProxy,
      timer,
      ExistingUrlsHandled(stateData.source.id, context.self),
      (
          remainingUrls: List[
            UrlWithPayLoad[SimpleEntryView[SimpleUrlView, ArticleTagView]]
          ],
          updatedUrlToActivation: Map[String, Long]
      ) =>
        handleExistingUrls(
          stateData,
          remainingUrls,
          updatedUrlToActivation,
          workerPoolProxy,
          timer,
          context
        )
    )

  /**
    * Handle queued urls. There are three possible scenarios:
    * 1) If the url handler replies with a failure, try to solve it, otherwise report completion. And try to issue a new
    *    url handling.
    * 2) If the url handler replies successfully, report completion. And try to issue a new url handling.
    * 3) If the [[SourceHandler]] re-scheduled a url handling, try to issue it, if rate limit allows to.
    *
    * @param stateData                Current state of the actor
    * @param urlsToBeHandled          List of queued elements
    * @param urlToActivation          Mapping from url to their activation times
    * @param workerPoolProxy          Reference to url worker pool proxy
    * @param timer                    A timer to re-schedule urls
    * @param completionMessage        Completion message to send, after handling finished
    * @param stateChangeOnActivation  State to apply, after a new url handling has been issued
    * @tparam U Type of queued url elements
    * @tparam P Type of payload of queued elements
    * @return Defined behavior.
    */
  private def handleUrls[U <: UrlQueueObject[P], P](
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[U],
      urlToActivation: Map[String, Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      timer: TimerScheduler[SourceHandlerMessage],
      completionMessage: SupervisorMessage,
      stateChangeOnActivation: (List[U], Map[String, Long]) => Behavior[
        SourceHandlerMessage
      ]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, UrlHandledWithFailure(url, urlId, payLoad, error)) =>
        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl == url
        }
        /* Define, what happens if the rate limit is exceeded */
        val handleRateLimitExceeding = () => {
          val (newlyStashedUrls, hasBeenRescheduled) = tryToReScheduleUrl(
            stateData.stashedUrls,
            stateData.maxRetries,
            url,
            urlId,
            stateData.repeatDelay,
            timer,
            payLoad
          )
          if (!hasBeenRescheduled) {
            context.log.warn(
              "The url '{}' ({}) already has been scheduled {} times. Give up on it.",
              url,
              urlId,
              stateData.maxRetries
            )
          }
          Some(stateData.copy(stashedUrls = newlyStashedUrls))
        }
        /* Log the error and handle accordingly. */
        val updateStateData = logAndHandleUrlHandlingFailure(
          url,
          error,
          context.log,
          handleRateLimitExceeding
        ).getOrElse(stateData)

        maybeIssueNextUnhandledUrl[U, P](
          context,
          workerPoolProxy,
          stateData.supervisor,
          timer,
          updateStateData,
          urlsToBeHandled,
          remainingActiveUrls,
          completionMessage,
          stateChangeOnActivation
        )
      case (context, UrlHandledSuccessfully(url)) =>
        context.log
          .debug("The url '{}' has been handled successfully.", url)

        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl == url
        }

        maybeIssueNextUnhandledUrl[U, P](
          context,
          workerPoolProxy,
          stateData.supervisor,
          timer,
          stateData,
          urlsToBeHandled,
          remainingActiveUrls,
          completionMessage,
          stateChangeOnActivation
        )

      case (context, ScheduleUrl(urlId, url, payLoad: Option[P])) =>
        context.log.debug(
          "I'm ask to reschedule the url '{}'. Check rate limit and if applicable, send out request to my worker pool.",
          url
        )
        maybeIssueNewHandling[U, P](
          context,
          workerPoolProxy,
          stateData.supervisor,
          timer,
          url,
          urlId,
          payLoad,
          urlsToBeHandled,
          urlToActivation,
          stateData,
          stateChangeOnActivation
        )

      case (context, ScheduleUrl(_, url, _)) =>
        context.log.debug(
          "I'm ask to reschedule the url '{}' with unsupported payload. Cannot do this.",
          url
        )
        Behaviors.same

      case _ => Behaviors.unhandled
    }

  /**
    * Checks the current amount of retries for this url and, if the maximum amount of retries is not exceeded,
    * re-schedule the url. Each time, the repeat delay is enlarged by multiplying it with the repeat delay. The boolean
    * return part indicates, if the url has been re-scheduled.
    *
    * @param urlToNumOfRetries  Mapping from url id to current amount of retries
    * @param maxRetries         Maximum permissible amount of retries
    * @param url                Actual url information
    * @param urlId              Identifier of the url
    * @param payLoad            Possible, additional payload for scheduling of an url
    * @param repeatDelay        Repeat delay
    * @return The updated mapping from url to retry and a boolean indicator, if that url has been re-scheduled
    */
  private def tryToReScheduleUrl(
      urlToNumOfRetries: Map[String, Int],
      maxRetries: Int,
      url: String,
      urlId: String,
      repeatDelay: Duration,
      timer: TimerScheduler[SourceHandlerMessage],
      payLoad: Option[_] = None
  ): (Map[String, Int], Boolean) = {
    val numOfRetries = urlToNumOfRetries.getOrElse(urlId, 0)
    if (numOfRetries < maxRetries) {
      /* ReSchedule url and register it */
      val currentRetry = numOfRetries + 1
      timer.startSingleTimer(
        ScheduleUrl(urlId, url, payLoad),
        FiniteDuration(currentRetry * repeatDelay.toMillis, "ms")
      )
      (urlToNumOfRetries + (urlId -> currentRetry), true)
    } else {
      /* Already tried often enough */
      (urlToNumOfRetries - urlId, false)
    }
  }

  /**
    * Log the error when handling an url and handle accordingly in the case of a rate limit exceeding
    *
    * @param url                      Url, that was handled unsuccessfully
    * @param failure                  Reported failure
    * @param logger                   Instance of logger to use
    * @param rateLimitExceededAction  Action to take care of, if rate limit is exceeded
    */
  private def logAndHandleUrlHandlingFailure(
      url: String,
      failure: Throwable,
      logger: Logger,
      rateLimitExceededAction: () => Option[SourceHandlerStateData]
  ): Option[SourceHandlerStateData] = failure match {
    case httpException: HttpStatusException
        if httpException.getStatusCode == 404 =>
      logger.warn(
        "The url '{}' doesn't exist (HTTP Status 404). Consider removing it.",
        url
      )
      None
    case httpException: HttpStatusException
        if httpException.getStatusCode == 403 =>
      logger.warn(
        "The url '{}' replied, that the rate limit is exceeded (HTTP Status 403). Consider adapting " +
          "configuration, especially the rate limit. Will re-schedule the visit of this url.",
        url
      )
      rateLimitExceededAction()
    case unknown =>
      logger.error(
        "Handling the url '{}' failed with unknown error. Skip it.\n\tError: {} - \"{}\"",
        url,
        unknown.getClass.getSimpleName,
        unknown.getMessage
      )
      None
  }

  /**
    * Check out the not yet handled urls and attempt to handle one if applicable
    *
    * @param context            Actor context, the actor is in
    * @param workerPoolProxy    Reference to the proxy for the worker pool
    * @param supervisor         Reference to the supervisor
    * @param timer              Timer for message scheduling
    * @param stateData          Current state of the actor
    * @param unhandledUrls      List of remaining urls
    * @param urlToActivation    Mapping from active url to activation time
    * @param completionMessage  Message, that indicates completion to source handler
    * @param stateChangeOnActivation  State, that shall be used after activation of new url
    * @return
    */
  private def maybeIssueNextUnhandledUrl[U <: UrlQueueObject[P], P](
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      timer: TimerScheduler[SourceHandlerMessage],
      stateData: SourceHandlerStateData,
      unhandledUrls: List[U],
      urlToActivation: Map[String, Long],
      completionMessage: SupervisorMessage,
      stateChangeOnActivation: (List[U], Map[String, Long]) => Behavior[
        SourceHandlerMessage
      ]
  ): Behavior[SourceHandlerMessage] =
    if (unhandledUrls.nonEmpty) {
      /* Trigger new runs */
      val (maybeNextUrl, remainingUrls) =
        nextUrl[U, P](unhandledUrls, context.log)
      maybeNextUrl match {
        case Some(nextUrl) =>
          context.log.debug(
            "Still {} urls remaining to be handled. Check, if rate limit allows for another issue.",
            remainingUrls.size
          )
          maybeIssueNewHandling(
            context,
            workerPoolProxy,
            supervisor,
            timer,
            nextUrl.name.get,
            nextUrl.id,
            nextUrl.payload,
            remainingUrls,
            urlToActivation,
            stateData,
            stateChangeOnActivation
          )
        case None =>
          context.log.info(
            "All urls of source '{}' ({}) are handled. Report to supervisor.",
            stateData.source.name.getOrElse(""),
            stateData.source.id
          )
          stateData.supervisor ! completionMessage
          /* Change to idle and wait for termination message */
          idle(stateData, workerPoolProxy)
      }
    } else if (urlToActivation.nonEmpty) {
      context.log.info(
        "No more unhandled urls for source '{}' ({}). Wait for the last {} active ones{}",
        stateData.source.name.getOrElse(""),
        stateData.source.id,
        urlToActivation.size,
        if (urlToActivation.size < 6) {
          "\n\t" + urlToActivation.keys.mkString("\n\t")
        } else {
          ""
        }
      )
      stateChangeOnActivation(List.empty, urlToActivation)
    } else {
      context.log.info(
        "All new urls of source '{}' ('{}') are handled. Report to supervisor.",
        stateData.source.id,
        stateData.source.name.getOrElse("")
      )
      stateData.supervisor ! completionMessage
      /* Change to idle and wait for termination message */
      idle(stateData, workerPoolProxy)
    }

  /**
    * Get the next url, as long as it has a name available
    *
    * @param urls   A view onto a url
    * @param logger A logger to log logging stuff
    * @return A tuple of a possibly apparent next entry and the remaining entries
    */
  @tailrec
  private def nextUrl[U <: UrlQueueObject[P], P](
      urls: List[U],
      logger: Logger
  ): (Option[U], List[U]) = {
    val (peakUrls, remainingUrls) = peek(urls, 1)
    peakUrls.headOption match {
      case Some(peakUrl) if peakUrl.name.nonEmpty =>
        (Some(peakUrl), remainingUrls)
      case Some(peakUrl) =>
        logger.error(
          "The url entry with id '{}' does not contain an actual, visitable url. Cannot handle this.",
          peakUrl.id
        )
        nextUrl[U, P](remainingUrls, logger)
      case None =>
        (None, remainingUrls)
    }
  }

  /**
    * If possible (sticking to a rate limit), issue a new url handling and change to applicable state.
    *
    * @param context                  Actor context, the actor is in
    * @param workerPoolProxy          Reference to the proxy for the worker pool
    * @param supervisor               Reference to the supervisor
    * @param timer                    Timer for message scheduling
    * @param targetUrl                The targeted url
    * @param targetUrlId              Id of the targeted url in database
    * @param remainingUrls            Remaining urls to handle
    * @param urlToActivation          Mapping from active url to it's activation time.
    * @param stateData                Current state of the actor
    * @param stateChangeOnActivation  State, that shall be used after activation of new url
    * @return Defined behavior
    */
  private def maybeIssueNewHandling[U <: UrlQueueObject[P], P](
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      timer: TimerScheduler[SourceHandlerMessage],
      targetUrl: String,
      targetUrlId: String,
      maybePayload: Option[P],
      remainingUrls: List[U],
      urlToActivation: Map[String, Long],
      stateData: SourceHandlerStateData,
      stateChangeOnActivation: (List[U], Map[String, Long]) => Behavior[
        SourceHandlerMessage
      ]
  ): Behavior[SourceHandlerMessage] = {
    /* Figure out, which issue dates are within the last allowed duration */
    val currentInstant = System.currentTimeMillis()
    val firstRelevantInstant = currentInstant - stateData.repeatDelay.toMillis
    val relevantIssues =
      urlToActivation.values.filter(_ >= firstRelevantInstant)
    if (relevantIssues.size < stateData.workerPoolSize) {
      context.log.debug(
        "Rate limit {}/{} Hz is not exceeded. Issue a new url handling.",
        stateData.workerPoolSize,
        stateData.repeatDelay.toMillis / 1000
      )
      val updatedUrlToActivation = urlToActivation + (targetUrl -> currentInstant)
      workerPoolProxy ! HandleNewUrl(
        targetUrl,
        targetUrlId,
        stateData.pageProfile,
        context.self
      )
      stateChangeOnActivation(remainingUrls, updatedUrlToActivation)

    } else {
      context.log.debug(
        "Rate limit {}/{} Hz currently is exceeded. Wait {}s with a new issue of url handling.",
        stateData.workerPoolSize,
        stateData.repeatDelay.toMillis / 1000,
        stateData.repeatDelay.toMillis / 1000
      )

      /* This is a first try. Therefore, no registration of re-tries is needed here! */
      val waitTimeOut = FiniteDuration(stateData.repeatDelay.toMillis, "ms")
      timer.startSingleTimer(
        ScheduleUrl(targetUrlId, targetUrl, maybePayload),
        waitTimeOut
      )
      Behaviors.same
    }
  }
}

object UrlHandlingSupport {
  sealed trait UrlQueueObject[P] {
    def id: String
    def name: Option[String]
    def payload: Option[P]
  }

  final class SimpleUrl private (urlId: String, url: Option[String])
      extends UrlQueueObject[Nothing] {
    override def id: String = urlId
    override def name: Option[String] = url
    override def payload: Option[Nothing] = None
  }
  object SimpleUrl {
    def apply(simpleUrlView: SimpleUrlView) =
      new SimpleUrl(simpleUrlView.id, simpleUrlView.name)
  }

  final class UrlWithPayLoad[P](
      urlId: String,
      url: Option[String],
      payLoad: Option[P]
  ) extends UrlQueueObject[P] {
    override def id: String = urlId
    override def name: Option[String] = url
    override def payload: Option[P] = payLoad
  }
  object UrlWithPayLoad {
    def apply[P](tuple: (SimpleUrlView, Option[P])): UrlWithPayLoad[P] =
      tuple match {
        case (simpleUrlView, payLoad) =>
          new UrlWithPayLoad[P](simpleUrlView.id, simpleUrlView.name, payLoad)
      }
  }
}
