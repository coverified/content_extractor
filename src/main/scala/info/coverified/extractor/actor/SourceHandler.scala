/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  Routers,
  TimerScheduler
}
import info.coverified.extractor.actor.SourceHandler.{
  SourceHandlerStateData,
  nextUrl,
  peek
}
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage,
  UrlHandlerMessage
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
  NewUrlHandledSuccessfully,
  NewUrlHandledWithFailure,
  ReScheduleUrl,
  Run
}
import info.coverified.extractor.messages.SupervisorMessage.{
  SourceHandled,
  SourceHandlerInitialized
}
import info.coverified.extractor.messages.UrlHandlerMessage.HandleNewUrl
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.jsoup.HttpStatusException
import org.slf4j.Logger
import sttp.model.Uri

import java.time.Duration
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

/**
  * An actor, that handles the extraction process per source
  */
class SourceHandler(private val timer: TimerScheduler[SourceHandlerMessage]) {
  def uninitialized: Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (
          context,
          InitSourceHandler(
            apiUri,
            pageProfile,
            reAnalysisInterval,
            authSecret,
            chunkSize,
            repeatDelay,
            source,
            replyTo
          )
          ) =>
        context.log.info(
          "Initializing a source handler for source '{}' (to be found at '{}').",
          source.id,
          source.url
        )
        val stateData = SourceHandlerStateData(
          apiUri,
          pageProfile,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay,
          source,
          replyTo
        )
        replyTo ! SourceHandlerInitialized(source.id, context.self)
        idle(stateData)
      case _ => Behaviors.unhandled
    }

  /**
    * Waiting for anything to do
    *
    * @param stateData Current state of the actor
    * @return The defined behavior
    */
  def idle(
      stateData: SourceHandlerStateData
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, Run(replyTo)) =>
        context.log.info("Got asked to start activity.")

        context.log.info(
          "Start to analyse new, not yet visited urls for source '{}' ('{}').",
          stateData.source.id,
          stateData.source.name
        )
        /* Determine all new urls */
        new GraphQLHelper(stateData.apiUri, stateData.authSecret)
          .queryNewUrls(stateData.source.id) match {
          case Some(newUrls) if newUrls.isEmpty =>
            context.log.info("Found no new urls.")
            // TODO Change over to handling existing urls
            replyTo ! SourceHandled(stateData.source.id)
            Behaviors.stopped
          case Some(newUrls) =>
            context.log.info(
              "Found {} not yet visited urls for source '{}' ('{}')",
              newUrls.size,
              stateData.source.id,
              stateData.source.name.getOrElse("")
            )
            /* Set up a pool of workers to handle url */
            context.log.debug(
              "Starting an url worker pool of {} workers.",
              stateData.chunkSize
            )
            val urlWorkerPool = Routers
              .pool(poolSize = stateData.chunkSize) {
                Behaviors
                  .supervise(UrlHandler())
                  .onFailure(SupervisorStrategy.restart)
              }
              .withRoundRobinRouting()
            val urlWorkerProxy =
              context.spawn(
                urlWorkerPool,
                "UrlWorkerPool_" + stateData.source.id
              )

            val (firstBatch, remainingNewUrls) =
              peek(newUrls, stateData.chunkSize)
            /* Activate workers for the first batch and register the starting times */
            val urlActivationTimeStamps = firstBatch.flatMap { url =>
              url.name match {
                case Some(actualUrl) =>
                  urlWorkerProxy ! HandleNewUrl(
                    actualUrl,
                    stateData.pageProfile,
                    context.self
                  )
                  Some(System.currentTimeMillis())
                case None =>
                  context.log.error(
                    "The url entry with id '{}' doesn't contain a url to visit.",
                    url.id
                  )
                  None
              }
            }

            handleNewUrls(
              stateData,
              remainingNewUrls,
              urlActivationTimeStamps,
              urlWorkerProxy,
              replyTo
            )
          case None =>
            context.log.error(
              "Received malformed reply when requesting new urls."
            )
            replyTo ! SourceHandled(stateData.source.id)
            Behaviors.stopped
        }
      case _ => Behaviors.unhandled
    }

  /**
    * Behavior to steer the handling of new urls
    * @param stateData        Current state of the actor
    * @param urlsToBeHandled  List of new urls to be visited
    * @param workerPoolProxy  Reference to the worker pool proxy
    * @param supervisor       Reference to the supervisor
    * @return The defined behavior
    */
  def handleNewUrls(
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[SimpleUrlView],
      scrapeTimeStamps: List[Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, NewUrlHandledWithFailure(url, error)) =>
        error match {
          case httpException: HttpStatusException
              if httpException.getStatusCode == 404 =>
            context.log.warn(
              "The url '{}' doesn't exist (HTTP Status 404). Consider removing it.",
              url
            )
          case httpException: HttpStatusException
              if httpException.getStatusCode == 403 =>
            context.log.warn(
              "The url '{}' replied, that the rate limit is exceeded (HTTP Status 403). Consider adapting configuration, especially the rate limit. Will re-schedule the visit of this url in {} s.",
              url,
              stateData.repeatDelay.toMillis / 1000
            )
            timer.startSingleTimer(
              ReScheduleUrl(url),
              FiniteDuration(stateData.repeatDelay.toMillis, "ms")
            )
          case _ =>
            context.log.error(
              "Handling the url '{}' failed with unknown error. Skip it.",
              url
            )
        }
        maybeIssueNextUnhandledUrl(
          context,
          workerPoolProxy,
          supervisor,
          stateData,
          urlsToBeHandled,
          scrapeTimeStamps
        )
        Behaviors.same
      case (context, NewUrlHandledSuccessfully(url)) =>
        context.log.debug("The new url '{}' has been handled.", url)

        /* Remove the finished url from list of to be handled urls */
        val unhandledUrls = urlsToBeHandled.filterNot(_.name.contains(url))
        maybeIssueNextUnhandledUrl(
          context,
          workerPoolProxy,
          supervisor,
          stateData,
          unhandledUrls,
          scrapeTimeStamps
        )

      case (context, ReScheduleUrl(url)) =>
        context.log.debug(
          "I'm ask to reschedule the url '{}'. Check rate limit and if applicable, send out request to my worker pool.",
          url
        )
        maybeIssueNewHandling(
          context,
          workerPoolProxy,
          supervisor,
          url,
          scrapeTimeStamps,
          urlsToBeHandled,
          stateData
        )
      case _ => Behaviors.unhandled
    }

  /**
    * Check out the not yet handled urls and attempt to handle one if applicable
    *
    * @param context          Actor context, the actor is in
    * @param workerPoolProxy  Reference to the proxy for the worker pool
    * @param supervisor       Reference to the supervisor
    * @param stateData        Current state of the actor
    * @param unhandledUrls    List of remaining urls
    * @param scrapeTimeStamps A list of time stamps, where lastly scrapes have started.
    * @return
    */
  def maybeIssueNextUnhandledUrl(
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      stateData: SourceHandlerStateData,
      unhandledUrls: List[SimpleUrlView],
      scrapeTimeStamps: List[Long]
  ): Behavior[SourceHandlerMessage] =
    if (unhandledUrls.nonEmpty) {
      /* Trigger new runs */
      val (maybeNextUrl, remainingUrls) =
        nextUrl(unhandledUrls, context.log)
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
            nextUrl.name.get,
            scrapeTimeStamps,
            remainingUrls,
            stateData
          )
        case None =>
          context.log.info(
            "All new urls of source '{}' ('{}') are handled. Shut down workers and myself.",
            stateData.source.id,
            stateData.source.name.getOrElse("")
          )
          context.stop(workerPoolProxy)
          supervisor ! SourceHandled(stateData.source.id)
          Behaviors.stopped
      }
    } else {
      context.log.info(
        "All new urls of source '{}' ('{}') are handled. Shut down workers and myself.",
        stateData.source.id,
        stateData.source.name.getOrElse("")
      )
      context.stop(workerPoolProxy)
      supervisor ! SourceHandled(stateData.source.id)
      Behaviors.stopped
    }

  /**
    * If possible (sticking to a rate limit), issue a new url handling and change to applicable state.
    *
    * @param context          Actor context, the actor is in
    * @param workerPoolProxy  Reference to the proxy for the worker pool
    * @param supervisor       Reference to the supervisor
    * @param targetUrl        The targeted url
    * @param scrapeTimeStamps A list of time stamps, where lastly scrapes have started.
    * @param remainingUrls    List of remaining urls
    * @param stateData        Current state of the actor
    * @return Defined behavior
    */
  def maybeIssueNewHandling(
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      targetUrl: String,
      scrapeTimeStamps: List[Long],
      remainingUrls: List[SimpleUrlView],
      stateData: SourceHandlerStateData
  ): Behavior[SourceHandlerMessage] = {
    /* Figure out, which issue dates are within the last allowed duration */
    val currentInstant = System.currentTimeMillis()
    val firstRelevantInstant = currentInstant - stateData.repeatDelay.toMillis
    val relevantIssues = scrapeTimeStamps.filter(_ >= firstRelevantInstant)
    if (relevantIssues.size < stateData.chunkSize) {
      context.log.debug(
        "Rate limit {}/{} Hz is not exceeded. Issue a new url handling.",
        stateData.chunkSize,
        stateData.repeatDelay.toMillis / 1000
      )
      val currentIssues = relevantIssues ++ List(currentInstant)
      workerPoolProxy ! HandleNewUrl(
        targetUrl,
        stateData.pageProfile,
        context.self
      )
      handleNewUrls(
        stateData,
        remainingUrls,
        currentIssues,
        workerPoolProxy,
        supervisor
      )
    } else {
      context.log.debug(
        "Rate limit {}/{} Hz currently is exceeded. Wait {}s with a new issue of url handling.",
        stateData.chunkSize,
        stateData.repeatDelay.toMillis / 1000,
        stateData.repeatDelay.toMillis / 1000
      )
      val waitTimeOut = FiniteDuration(stateData.repeatDelay.toMillis, "ms")
      timer.startSingleTimer(ReScheduleUrl(targetUrl), waitTimeOut)
      Behaviors.same
    }
  }
}

object SourceHandler {
  def apply(): Behavior[SourceHandlerMessage] =
    Behaviors.withTimers(timer => new SourceHandler(timer).uninitialized)

  final case class SourceHandlerStateData(
      apiUri: Uri,
      pageProfile: ProfileConfig,
      reAnalysisInterval: Duration,
      authSecret: String,
      chunkSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      supervisor: ActorRef[SupervisorMessage]
  )

  def peek[A](list: List[A], first: Int): (List[A], List[A]) = {
    val peek = list.take(first)
    (peek, list.filterNot(peek.contains(_)))
  }

  /**
    * Get the next url, as long as it has a name available
    *
    * @param urls   A view onto a url
    * @param logger A logger to log logging stuff
    * @return A tuple of a possibly apparent next entry and the remaining entries
    */
  @tailrec
  def nextUrl(
      urls: List[SimpleUrlView],
      logger: Logger
  ): (Option[SimpleUrlView], List[SimpleUrlView]) = {
    val (peakUrls, remainingUrls) = peek(urls, 1)
    peakUrls.headOption match {
      case Some(peakUrl) if peakUrl.name.nonEmpty =>
        (Some(peakUrl), remainingUrls)
      case Some(peakUrl) =>
        logger.error(
          "The url entry with id '' does not contain an actual, visitable url. Cannot handle this.",
          peakUrl.id
        )
        nextUrl(remainingUrls, logger)
      case None =>
        (None, remainingUrls)
    }
  }
}
