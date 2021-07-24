/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.{Behaviors, Routers}
import info.coverified.extractor.actor.SourceHandler.peek
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage,
  UrlHandlerMessage
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
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
import sttp.model.Uri

import java.time.Duration

/**
  * An actor, that handles the extraction process per source
  */
class SourceHandler {
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
            firstBatch.foreach { url =>
              url.name match {
                case Some(actualUrl) =>
                  urlWorkerProxy ! HandleNewUrl(
                    actualUrl,
                    stateData.pageProfile,
                    context.self
                  )
                case None =>
                  context.log.error(
                    "The url entry with id '{}' doesn't contain a url to visit.",
                    url.id
                  )
              }
            }

            handleNewUrls(stateData, remainingNewUrls, urlWorkerProxy, replyTo)
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
    * @param newUrls          List of new urls to be visited
    * @param workerPoolProxy  Reference to the worker pool proxy
    * @param supervisor       Reference to the supervisor
    * @return The defined behavior
    */
  def handleNewUrls(
      stateData: SourceHandlerStateData,
      newUrls: List[SimpleUrlView],
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, newUrlReply) =>
        context.log.debug("The new url has been handled.")
        /* TODO:
         *  1) Interpret, what has been sent
         *  2) Trigger new runs
         *  3) Re-Schedule missed trials
         */
        supervisor ! SourceHandled(stateData.source.id)
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

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
}

object SourceHandler {
  def apply(): Behavior[SourceHandlerMessage] =
    new SourceHandler().uninitialized

  def peek[A](list: List[A], first: Int): (List[A], List[A]) = {
    val peek = list.take(first)
    (peek, list.filterNot(peek.contains(_)))
  }
}
