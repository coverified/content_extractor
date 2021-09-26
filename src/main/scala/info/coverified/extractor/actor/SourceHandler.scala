/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.{Behaviors, Routers, TimerScheduler}
import info.coverified.extractor.actor.SourceHandler.{
  SourceHandlerInitializingStateData,
  SourceHandlerStateData,
  peek
}
import info.coverified.extractor.actor.UrlHandlingSupport.{
  SimpleUrl,
  UrlWithPayLoad
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
  MutatorInitialized
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
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import sttp.model.Uri

import java.time.{Duration, ZoneId}

/**
  * An actor, that handles the extraction process per source
  */
class SourceHandler(private val timer: TimerScheduler[SourceHandlerMessage])
    extends UrlHandlingSupport {
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
            maxRetries,
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

        val stateData = SourceHandlerInitializingStateData(
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
          maxRetries,
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
          case _: InitUrlHandler           => true
          case UrlHandlerMessage.Terminate => true
          case _                           => false
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
        initializingStateData.targetTimeZone,
        initializingStateData.apiUri,
        initializingStateData.authSecret
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
            replyTo ! NewUrlsHandled(
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
            replyTo ! NewUrlsHandled(
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

            val queueEntries = remainingNewUrls.map(SimpleUrl(_))
            handleNewUrls(
              stateData,
              queueEntries,
              urlToActivation,
              workerPoolProxy,
              timer,
              context
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
              "Found {} urls to be re-analyzed for source '{}' ({}).",
              relevantUrls.size,
              stateData.source.id,
              stateData.source.name.getOrElse("")
            )

            /* Select the first batch, send them to the url workers and change state accordingly */
            val (firstBatch, remainingUrls) =
              peek(relevantUrls, stateData.workerPoolSize)
            val urlToActivation = firstBatch.flatMap {
              case url if url.name.nonEmpty =>
                /* Send out messages to worker */
                val actualUrl = url.name.get
                workerPoolProxy ! HandleExistingUrl(
                  actualUrl,
                  url.id,
                  None,
                  stateData.pageProfile,
                  ctx.self
                )
                Some(actualUrl -> System.currentTimeMillis())
              case _ =>
                /* No actual url known, nothing to handle */
                None
            }.toMap

            /* Change state to handle existing urls */
            val queueEntries = remainingUrls.map(
              urlView =>
                UrlWithPayLoad[SimpleEntryView[SimpleUrlView, ArticleTagView]](
                  urlView,
                  None
                )
            )
            handleExistingUrls(
              stateData,
              queueEntries,
              urlToActivation,
              workerPoolProxy,
              timer,
              ctx
            )
        }

      case (ctx, SourceHandlerMessage.Terminate) =>
        ctx.log.info(
          "Termination requested for source handler '{}'. Shut down mutator and worker pool.",
          stateData.source.id
        )
        workerPoolProxy ! UrlHandlerMessage.Terminate
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
}

object SourceHandler {
  def apply(): Behavior[SourceHandlerMessage] =
    Behaviors.withTimers(timer => new SourceHandler(timer).uninitialized)

  final case class SourceHandlerInitializingStateData(
      userAgent: String,
      browseTimeout: Duration,
      targetDateTimePattern: String,
      targetTimeZone: ZoneId,
      apiUri: Uri,
      authSecret: String,
      pageProfile: ProfileConfig,
      reAnalysisInterval: Duration,
      workerPoolSize: Int,
      repeatDelay: Duration,
      maxRetries: Int,
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
      maxRetries: Int,
      source: SourceView,
      mutator: ActorRef[MutatorMessage],
      supervisor: ActorRef[SupervisorMessage],
      stashedUrls: Map[String, Int] = Map.empty
  )
  object SourceHandlerStateData {
    def apply(initStateData: SourceHandlerInitializingStateData) =
      new SourceHandlerStateData(
        new GraphQLHelper(initStateData.apiUri, initStateData.authSecret),
        initStateData.pageProfile,
        initStateData.reAnalysisInterval,
        initStateData.workerPoolSize,
        initStateData.repeatDelay,
        initStateData.maxRetries,
        initStateData.source,
        initStateData.mutator,
        initStateData.supervisor
      )
  }

  def peek[A](list: List[A], first: Int): (List[A], List[A]) = {
    val peek = list.take(first)
    (peek, list.filterNot(peek.contains(_)))
  }
}
