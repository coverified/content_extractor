/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.{ActorRef, Behavior, SupervisorStrategy}
import akka.actor.typed.scaladsl.{Behaviors, Routers}
import akka.stream.ActorAttributes.SupervisionStrategy
import info.coverified.extractor.messages.{
  SourceHandlerMessage,
  SupervisorMessage,
  UrlHandlerMessage
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
  NewUrlHandled,
  Run
}
import info.coverified.extractor.messages.SupervisorMessage.{
  SourceHandled,
  SourceHandlerInitialized
}
import info.coverified.extractor.messages.UrlHandlerMessage.HandleNewUrl
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
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
            profileDirectoryPath,
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
          profileDirectoryPath,
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

        /* Set up a pool of workers to handle url */
        val urlWorkerPool = Routers
          .pool(poolSize = stateData.chunkSize) {
            Behaviors
              .supervise(UrlHandler())
              .onFailure(SupervisorStrategy.restart)
          }
          .withRoundRobinRouting()
        val urlWorkerProxy =
          context.spawn(urlWorkerPool, "UrlWorkerPool_" + stateData.source.id)
        urlWorkerProxy ! HandleNewUrl("foo", context.self)
        handleNewUrls(stateData, urlWorkerProxy, replyTo)
      case _ => Behaviors.unhandled
    }

  /**
    * Behavior to steer the handling of new urls
    * @param stateData        Current state of the actor
    * @param workerPoolProxy  Reference to the worker pool proxy
    * @param supervisor       Reference to the supervisor
    * @return The defined behavior
    */
  def handleNewUrls(
      stateData: SourceHandlerStateData,
      workerPoolProxy: ActorRef[UrlHandlerMessage],
      supervisor: ActorRef[SupervisorMessage]
  ): Behaviors.Receive[SourceHandlerMessage] =
    Behaviors.receive[SourceHandlerMessage] {
      case (context, NewUrlHandled(url)) =>
        context.log.debug("The new url '{}' has been handled.", url)
        supervisor ! SourceHandled(stateData.source.id)
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  final case class SourceHandlerStateData(
      apiUri: Uri,
      profileDirectoryPath: String,
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
}
