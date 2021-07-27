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
  SourceHandlerInitializingStateData,
  SourceHandlerStateData,
  nextUrl,
  peek
}
import info.coverified.extractor.messages.MutatorMessage.{
  InitMutator,
  Terminate
}
import info.coverified.extractor.messages.{
  MutatorMessage,
  SourceHandlerMessage,
  SupervisorMessage,
  UrlHandlerMessage
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  HandleExistingUrls,
  HandleNewUrls,
  InitSourceHandler,
  MutationsCompleted,
  MutatorInitialized,
  ReScheduleUrl,
  UrlHandledSuccessfully,
  UrlHandledWithFailure
}
import info.coverified.extractor.messages.SupervisorMessage.{
  ExistingUrlsHandled,
  NewUrlsHandled,
  SourceHandlerInitialized,
  SourceHandlerTerminated
}
import info.coverified.extractor.messages.UrlHandlerMessage.{
  HandleExistingUrl,
  HandleNewUrl,
  InitUrlHandler
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.jsoup.HttpStatusException
import org.slf4j.Logger

import java.time.{Duration, ZoneId}
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
            userAgent,
            browseTimeout,
            targetDateTimePattern,
            targetTimeZone,
            apiUri,
            authSecret,
            pageProfile,
            reAnalysisInterval,
            workerPoolSize,
            repeatDelay,
            source,
            distinctTagHandler,
            replyTo
          )
          ) =>
        context.log.info(
          "Initializing a source handler for source '{}' ({}).",
          source.name.getOrElse(""),
          source.id
        )

        /* Start a mutator */
        val mutator = context.spawn(Mutator(), "Mutator_" + source.id)
        mutator ! InitMutator(
          apiUri,
          authSecret,
          reAnalysisInterval,
          distinctTagHandler,
          context.self
        )
        context.watchWith(mutator, MutationsCompleted)

        val graphQLHelper = new GraphQLHelper(apiUri, authSecret)
        val stateData = SourceHandlerInitializingStateData(
          userAgent,
          browseTimeout,
          targetDateTimePattern,
          targetTimeZone,
          graphQLHelper,
          pageProfile,
          reAnalysisInterval,
          workerPoolSize,
          repeatDelay,
          source,
          mutator,
          replyTo
        )
        initializing(stateData)
      case _ => Behaviors.unhandled
    }

  def initializing(
      initializingStateData: SourceHandlerInitializingStateData
  ): Behaviors.Receive[SourceHandlerMessage] = Behaviors.receive {
    case (ctx, MutatorInitialized) =>
      ctx.log.info(
        "Mutator for source '{}' ('{}') successfully initialized. Start worker pool of {} workers.",
        initializingStateData.source.id,
        initializingStateData.source.name.getOrElse(""),
        initializingStateData.workerPoolSize
      )

      /* Set up a pool of workers to handle url */
      val urlWorkerPool = Routers
        .pool(poolSize = initializingStateData.workerPoolSize) {
          Behaviors
            .supervise(UrlHandler())
            .onFailure(SupervisorStrategy.restart)
        }
        .withRoundRobinRouting()
        .withBroadcastPredicate {
          case _: InitUrlHandler => true
          case _                 => false
        }
      val urlWorkerProxy =
        ctx.spawn(
          urlWorkerPool,
          "UrlWorkerPool_" + initializingStateData.source.id
        )

      /* Broadcast reference to mutator to all workers */
      urlWorkerProxy ! InitUrlHandler(
        initializingStateData.mutator,
        initializingStateData.userAgent,
        initializingStateData.browseTimeout,
        initializingStateData.targetDateTimePattern,
        initializingStateData.targetTimeZone
      )

      /* Report completion of initialization */
      initializingStateData.supervisor ! SourceHandlerInitialized(
        initializingStateData.source.id,
        ctx.self
      )
      val stateData = SourceHandlerStateData(initializingStateData)
      idle(stateData, urlWorkerProxy)
    case _ => Behaviors.unhandled
  }

  /**
    * Waiting for anything to do
    *
    * @param stateData        Current state of the actor
    * @param workerPoolProxy  Proxy actor for worker pool
    * @return The defined behavior
    */
  def idle(
      stateData: SourceHandlerStateData,
      workerPoolProxy: ActorRef[UrlHandlerMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, HandleNewUrls(replyTo)) =>
        context.log.info(
          "Start to analyse new, not yet visited urls for source '{}' ({}).",
          stateData.source.name.getOrElse(""),
          stateData.source.id
        )
        /* Determine all new urls */
        stateData.graphQLHelper.queryNewUrls(stateData.source.id) match {
          case None =>
            context.log.error(
              "Received malformed reply when requesting new urls."
            )
            stateData.supervisor ! NewUrlsHandled(
              stateData.source.id,
              context.self
            )
            Behaviors.same
          case Some(newUrls) if newUrls.isEmpty =>
            context.log.info(
              "Found no new urls for source '{}' ({}). Report back to supervisor, that all new urls are handled.",
              stateData.source.name.getOrElse(""),
              stateData.source.id
            )
            stateData.supervisor ! NewUrlsHandled(
              stateData.source.id,
              context.self
            )
            /* Stay in idle and wait for termination message */
            Behaviors.same
          case Some(newUrls) =>
            context.log.info(
              "Found {} not yet visited urls for source '{}' ({}).",
              newUrls.size,
              stateData.source.id,
              stateData.source.name.getOrElse("")
            )
            val (firstBatch, remainingNewUrls) =
              peek(newUrls, stateData.workerPoolSize)
            /* Activate workers for the first batch and register the starting times */
            val urlToActivation = firstBatch.flatMap { url =>
              url.name match {
                case Some(actualUrl) =>
                  workerPoolProxy ! HandleNewUrl(
                    actualUrl,
                    url.id,
                    stateData.pageProfile,
                    context.self
                  )
                  Some(actualUrl -> System.currentTimeMillis())
                case None =>
                  context.log.error(
                    "The url entry with id '{}' doesn't contain a url to visit.",
                    url.id
                  )
                  None
              }
            }.toMap

            handleNewUrls(
              stateData,
              remainingNewUrls,
              urlToActivation,
              workerPoolProxy
            )
        }
      case (ctx, HandleExistingUrls(replyTo)) =>
        ctx.log.info(
          "Start to analyse existing, yet already visited urls for source '{}' ({}).",
          stateData.source.name.getOrElse(""),
          stateData.source.id
        )

        /* Get relevant information from API */
        stateData.graphQLHelper.queryExistingUrls(
          stateData.source.id,
          stateData.reAnalysisInterval
        ) match {
          case None =>
            ctx.log.error(
              "Received empty reply from API when requesting urls for re-analysis in source '{}' ({}). Report, that all urls to be re-analyzed are handled.",
              stateData.source.name.getOrElse(""),
              stateData.source.id
            )
            stateData.supervisor ! ExistingUrlsHandled(
              stateData.source.id,
              ctx.self
            )
            Behaviors.same
          case Some(relevantUrls) if relevantUrls.isEmpty =>
            ctx.log.info(
              "No existing urls to handle for source '{}' ({}). Report back to supervisor, that all urls to be re-analysed are handled.",
              stateData.source.name.getOrElse(""),
              stateData.source.id
            )
            stateData.supervisor ! ExistingUrlsHandled(
              stateData.source.id,
              ctx.self
            )
            Behaviors.same
          case Some(relevantUrls) =>
            ctx.log.debug(
              "Found {} urls to be re-analyzed for source '{}' ({}). Query matching entries.",
              relevantUrls.size,
              stateData.source.id,
              stateData.source.name.getOrElse("")
            )

            val urlIds = relevantUrls.map(_.id)
            stateData.graphQLHelper.queryMatchingEntries(urlIds) match {
              case Some(matchingEntries) =>
                ctx.log.info(
                  "Found {} urls to be re-analyzed for source '{}' ({}). {} of them already have an entry available.",
                  relevantUrls.size,
                  stateData.source.id,
                  stateData.source.name.getOrElse(""),
                  matchingEntries.size
                )

                /* "Zip" urls and entries and sort them accordingly. */
                val urlToEntry = relevantUrls.map { url =>
                  /* Try to find a matching entry for this url */
                  url -> matchingEntries.find { entry =>
                    /* Entry matches, if the related url has the queried id */
                    entry.url.exists(_.id == url.id)
                  }
                }

                /* Select the first batch, send them to the url workers and change state accordingly */
                val (firstBatch, remainingUrls) =
                  peek(urlToEntry, stateData.workerPoolSize)
                val urlToActivation = firstBatch.flatMap {
                  case (url, maybeEntry) if url.name.nonEmpty =>
                    /* Send out messages to worker */
                    workerPoolProxy ! HandleExistingUrl(
                      url.name.get,
                      url.id,
                      maybeEntry,
                      stateData.pageProfile,
                      ctx.self
                    )
                    Some(url -> System.currentTimeMillis())
                  case _ =>
                    /* No actual url known, nothing to handle */
                    None
                }.toMap

                /* Change state to handle existing urls */
                handleExistingUrls(
                  stateData,
                  remainingUrls,
                  urlToActivation,
                  workerPoolProxy
                )
              case None =>
                ctx.log.error(
                  "Received empty reply from API when requesting matching entries for yet visited urls in source " +
                    "'{}' ({}). Report, that all urls to be re-analyzed are handled.",
                  stateData.source.name.getOrElse(""),
                  stateData.source.id
                )
                stateData.supervisor ! ExistingUrlsHandled(
                  stateData.source.id,
                  ctx.self
                )
                Behaviors.same
            }
        }

      case (ctx, SourceHandlerMessage.Terminate) =>
        ctx.log.info(
          "Termination requested for source handler '{}'. Shut down mutator and worker pool.",
          stateData.source.id
        )
        ctx.stop(workerPoolProxy)
        stateData.mutator ! Terminate
        shutdown(stateData)
      case _ => Behaviors.unhandled
    }

  private def shutdown(
      stateData: SourceHandlerStateData
  ): Behaviors.Receive[SourceHandlerMessage] = Behaviors.receive {
    case (ctx, MutationsCompleted) =>
      ctx.log.info(
        s"Mutator terminated! Shutting down SourceHandler for '{}' ({}).",
        stateData.source.name.getOrElse(""),
        stateData.source.id
      )
      stateData.graphQLHelper.close()
      stateData.supervisor ! SourceHandlerTerminated(stateData.source.id)
      Behaviors.stopped
    case (ctx, invalid) =>
      ctx.log.error(
        s"Invalid message received during shutdown process: '$invalid"
      )
      Behaviors.same
  }

  /**
    * Behavior to steer the handling of new urls
    *
    * @param stateData       Current state of the actor
    * @param urlsToBeHandled Remaining urls to be handled
    * @param urlToActivation Mapping from activated url to it's activation time
    * @param workerPoolProxy Reference to the worker pool proxy
    * @return The defined behavior
    */
  def handleNewUrls(
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[SimpleUrlView],
      urlToActivation: Map[String, Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, UrlHandledWithFailure(url, urlId, error)) =>
        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl == url
        }
        /* Log the error and handle accordingly. */
        logAndHandleUrlHandlingFailure(url, error, context.log, () => {
          timer.startSingleTimer(
            ReScheduleUrl(url, urlId),
            FiniteDuration(stateData.repeatDelay.toMillis, "ms")
          )
        })

        maybeIssueNextUnhandledUrl(
          context,
          workerPoolProxy,
          stateData.supervisor,
          stateData,
          urlsToBeHandled,
          remainingActiveUrls
        )
      case (context, UrlHandledSuccessfully(url)) =>
        context.log
          .debug("The new url '{}' has been handled successfully.", url)

        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl == url
        }

        maybeIssueNextUnhandledUrl(
          context,
          workerPoolProxy,
          stateData.supervisor,
          stateData,
          urlsToBeHandled,
          remainingActiveUrls
        )

      case (context, ReScheduleUrl(url, urlId)) =>
        context.log.debug(
          "I'm ask to reschedule the url '{}'. Check rate limit and if applicable, send out request to my worker pool.",
          url
        )
        maybeIssueNewHandling(
          context,
          workerPoolProxy,
          stateData.supervisor,
          url,
          urlId,
          urlToActivation,
          urlsToBeHandled,
          stateData
        )
      case _ => Behaviors.unhandled
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
      rateLimitExceededAction: () => Unit
  ): Unit = failure match {
    case httpException: HttpStatusException
        if httpException.getStatusCode == 404 =>
      logger.warn(
        "The url '{}' doesn't exist (HTTP Status 404). Consider removing it.",
        url
      )
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
  }

  /**
    * Handle all existing entries
    *
    * @param stateData       Current state of the actor
    * @param urlsToBeHandled Remaining urls and entries to be handled
    * @param urlToActivation Mapping from activated url to it's activation time
    * @param workerPoolProxy Reference to the worker pool proxy
    * @return The defined behavior
    */
  def handleExistingUrls(
      stateData: SourceHandlerStateData,
      urlsToBeHandled: List[
        (SimpleUrlView, Option[SimpleEntryView[SimpleUrlView, String]])
      ],
      urlToActivation: Map[SimpleUrlView, Long],
      workerPoolProxy: ActorRef[UrlHandlerMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (ctx, UrlHandledSuccessfully(url)) =>
        ctx.log
          .debug(
            "The already visited url '{}' has been handled successfully.",
            url
          )

        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl.name.contains(url)
        }
        /* TODO: Add logic */
        stateData.supervisor ! ExistingUrlsHandled(
          stateData.source.id,
          ctx.self
        )
        idle(stateData, workerPoolProxy)
      case (ctx, UrlHandledWithFailure(url, urlId, failure)) =>
        /* Remove the reporter from list of active ones */
        val remainingActiveUrls = urlToActivation.filterNot {
          case (remainingUrl, _) => remainingUrl.name.contains(url)
        }
        /* Log the error and handle accordingly. */
        logAndHandleUrlHandlingFailure(
          url,
          failure,
          ctx.log,
          () => {
            timer.startSingleTimer(
              // TODO: Adapt re-scheduling message to accommodate the entry
              ReScheduleUrl(url, urlId),
              FiniteDuration(stateData.repeatDelay.toMillis, "ms")
            )
          }
        )
        /* TODO: Add logic */
        stateData.supervisor ! ExistingUrlsHandled(
          stateData.source.id,
          ctx.self
        )
        idle(stateData, workerPoolProxy)
      case _ => Behaviors.unhandled
    }

  /**
    * Check out the not yet handled urls and attempt to handle one if applicable
    *
    * @param context         Actor context, the actor is in
    * @param workerPoolProxy Reference to the proxy for the worker pool
    * @param supervisor      Reference to the supervisor
    * @param stateData       Current state of the actor
    * @param unhandledUrls   List of remaining urls
    * @param urlToActivation Mapping from active url to activation time
    * @return
    */
  def maybeIssueNextUnhandledUrl(
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      stateData: SourceHandlerStateData,
      unhandledUrls: List[SimpleUrlView],
      urlToActivation: Map[String, Long]
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
            nextUrl.id,
            urlToActivation,
            remainingUrls,
            stateData
          )
        case None =>
          context.log.info(
            "All new urls of source '{}' ('{}') are handled. Report to supervisor.",
            stateData.source.id,
            stateData.source.name.getOrElse("")
          )
          stateData.supervisor ! NewUrlsHandled(
            stateData.source.id,
            context.self
          )
          /* Change to idle and wait for termination message */
          idle(stateData, workerPoolProxy)
      }
    } else if (urlToActivation.nonEmpty) {
      context.log.info(
        "No more unhandled urls for source '{}' ('{}'). Wait for the last {} active ones{}",
        stateData.source.id,
        stateData.source.name.getOrElse(""),
        urlToActivation.size,
        if (urlToActivation.size < 6) {
          "\n\t" + urlToActivation.keys.mkString("\n\t")
        } else {
          ""
        }
      )
      handleNewUrls(
        stateData,
        unhandledUrls,
        urlToActivation,
        workerPoolProxy
      )
    } else {
      context.log.info(
        "All new urls of source '{}' ('{}') are handled. Report to supervisor.",
        stateData.source.id,
        stateData.source.name.getOrElse("")
      )
      stateData.supervisor ! NewUrlsHandled(stateData.source.id, context.self)
      /* Change to idle and wait for termination message */
      idle(stateData, workerPoolProxy)
    }

  /**
    * If possible (sticking to a rate limit), issue a new url handling and change to applicable state.
    *
    * @param context         Actor context, the actor is in
    * @param workerPoolProxy Reference to the proxy for the worker pool
    * @param supervisor      Reference to the supervisor
    * @param targetUrl       The targeted url
    * @param targetUrlId     Id of the targeted url in database
    * @param urlToActivation Mapping from active url to it's activation time.
    * @param remainingUrls   List of remaining urls
    * @param stateData       Current state of the actor
    * @return Defined behavior
    */
  def maybeIssueNewHandling(
      context: ActorContext[SourceHandlerMessage],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage],
      targetUrl: String,
      targetUrlId: String,
      urlToActivation: Map[String, Long],
      remainingUrls: List[SimpleUrlView],
      stateData: SourceHandlerStateData
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
      val updateUrlToActivation = urlToActivation + (targetUrl -> currentInstant)
      workerPoolProxy ! HandleNewUrl(
        targetUrl,
        targetUrlId,
        stateData.pageProfile,
        context.self
      )
      handleNewUrls(
        stateData,
        remainingUrls,
        updateUrlToActivation,
        workerPoolProxy
      )
    } else {
      context.log.debug(
        "Rate limit {}/{} Hz currently is exceeded. Wait {}s with a new issue of url handling.",
        stateData.workerPoolSize,
        stateData.repeatDelay.toMillis / 1000,
        stateData.repeatDelay.toMillis / 1000
      )
      val waitTimeOut = FiniteDuration(stateData.repeatDelay.toMillis, "ms")
      timer.startSingleTimer(ReScheduleUrl(targetUrl, targetUrlId), waitTimeOut)
      Behaviors.same
    }
  }
}

object SourceHandler {
  def apply(): Behavior[SourceHandlerMessage] =
    Behaviors.withTimers(timer => new SourceHandler(timer).uninitialized)

  final case class SourceHandlerInitializingStateData(
      userAgent: String,
      browseTimeout: Duration,
      targetDateTimePattern: String,
      targetTimeZone: ZoneId,
      graphQLHelper: GraphQLHelper,
      pageProfile: ProfileConfig,
      reAnalysisInterval: Duration,
      workerPoolSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      mutator: ActorRef[MutatorMessage],
      supervisor: ActorRef[SupervisorMessage]
  )

  final case class SourceHandlerStateData(
      graphQLHelper: GraphQLHelper,
      pageProfile: ProfileConfig,
      reAnalysisInterval: Duration,
      workerPoolSize: Int,
      repeatDelay: Duration,
      source: SourceView,
      mutator: ActorRef[MutatorMessage],
      supervisor: ActorRef[SupervisorMessage]
  )
  object SourceHandlerStateData {
    def apply(initStateData: SourceHandlerInitializingStateData) =
      new SourceHandlerStateData(
        initStateData.graphQLHelper,
        initStateData.pageProfile,
        initStateData.reAnalysisInterval,
        initStateData.workerPoolSize,
        initStateData.repeatDelay,
        initStateData.source,
        initStateData.mutator,
        initStateData.supervisor
      )
  }

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
