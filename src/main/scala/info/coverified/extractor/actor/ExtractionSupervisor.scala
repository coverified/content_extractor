/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors._
import com.typesafe.config.ConfigFactory
import info.coverified.extractor.messages.DistinctTagHandlerMessage.{
  InitializeDistinctTagHandler,
  Terminate
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
  HandleNewUrls
}
import info.coverified.extractor.messages.{
  DistinctTagHandlerMessage,
  SourceHandlerMessage,
  SupervisorMessage
}
import info.coverified.extractor.messages.SupervisorMessage.{
  DistinctTagHandlerInitialized,
  DistinctTagHandlerTerminated,
  InitSupervisor,
  NewUrlsHandled,
  SourceHandlerInitialized
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import sttp.model.Uri

import java.io.File
import java.net.URL
import java.time.Duration

object ExtractionSupervisor {
  def apply(): Behavior[SupervisorMessage] = uninitialized

  /**
    * Start uninitialized
    */
  def uninitialized: Receive[SupervisorMessage] = receive[SupervisorMessage] {
    case (context, initMessage: InitSupervisor) =>
      context.log.info("Received a init message: {}.", initMessage)

      /* Query all sources */
      val graphQLHelper =
        new GraphQLHelper(initMessage.apiUri, initMessage.authSecret)
      val maybeSources = graphQLHelper.queryAllSources
      graphQLHelper.close()
      maybeSources match {
        case Some(emptySources) if emptySources.isEmpty =>
          context.log.info("There are no sources available. I'm done!")
          Behaviors.stopped
        case None =>
          context.log.warn(
            "Querying sources did not return a sensible reply. Shut down."
          )
          Behaviors.stopped
        case Some(sources) =>
          context.log.info(
            "Received {} sources. Spawn an actor for each of them.",
            sources.size
          )

          val distinctTagHandlerRef = initializeDistinctTagHandler(
            context,
            initMessage.apiUri,
            initMessage.authSecret
          )

          /* Change state to await init responses */
          context.log.debug(
            "Wait for distinct tag handler to report completed initialization."
          )
          val stateData = InitializingStateData(
            initMessage.profileDirectoryPath,
            initMessage.apiUri,
            initMessage.authSecret,
            initMessage.reAnalysisInterval,
            initMessage.repeatDelay,
            initMessage.chunkSize,
            distinctTagHandlerRef,
            sources
          )
          initializing(stateData)
      }
    case _ => unhandled
  }

  def initializing(
      initStateData: InitializingStateData,
      tagHandlerInitialized: Boolean = false,
      awaitedSources: Map[String, ActorRef[SourceHandlerMessage]] = Map.empty,
      activeSources: Map[String, ActorRef[SourceHandlerMessage]] = Map.empty
  ): Receive[SupervisorMessage] = Behaviors.receive {
    case (ctx, DistinctTagHandlerInitialized) if !tagHandlerInitialized =>
      ctx.log.debug(
        "Distinct tag handler initialized. Build source handlers and initialize them."
      )

      val sourceIdToHandlerRef = initStateData match {
        case InitializingStateData(
            profileDirectoryPath,
            apiUri,
            authSecret,
            reAnalysisInterval,
            repeatDelay,
            chunkSize,
            distinctTagHandlerRef,
            sourcesToInitialize
            ) =>
          initializeChildren(
            ctx,
            distinctTagHandlerRef,
            profileDirectoryPath,
            apiUri,
            authSecret,
            reAnalysisInterval,
            repeatDelay,
            chunkSize,
            sourcesToInitialize
          )
      }

      if (sourceIdToHandlerRef.isEmpty) {
        ctx.log.warn("No source handler has been started. Exit.")
        Behaviors.stopped
      } else {
        /* There are sources, that have been initialized. Wait for their responses */
        initializing(
          initStateData,
          tagHandlerInitialized = true,
          sourceIdToHandlerRef
        )
      }

    case (ctx, SourceHandlerInitialized(sourceId, replyTo))
        if tagHandlerInitialized =>
      ctx.log.debug(
        "Source handler for source '{}' successfully initialized.",
        sourceId
      )
      val initializingSources = awaitedSources.filterNot {
        case (key, _) => key == sourceId
      }
      val newActiveSource = activeSources + (sourceId -> replyTo)
      if (initializingSources.nonEmpty) {
        ctx.log.debug(
          "Still initializing source handler for sources:\n\t{}",
          initializingSources.mkString("\n\t")
        )
        initializing(
          initStateData,
          tagHandlerInitialized = true,
          initializingSources,
          newActiveSource
        )
      } else {
        ctx.log.debug("All source handlers initialized. Trigger them to start!")
        newActiveSource.values.foreach(_ ! HandleNewUrls(ctx.self))

        val stateData = ExtractorStateData(
          initStateData.reAnalysisInterval,
          initStateData.chunkSize,
          initStateData.repeatDelay,
          initStateData.distinctTagHandlerRef
        )
        handleSourceResponses(
          stateData,
          initializingSources,
          newActiveSource
        )
      }

    case (ctx, _: SourceHandlerInitialized) if !tagHandlerInitialized =>
      ctx.log.error(
        "Initialization of supervisor failed. Received an init completion from source handler, although it wasn't meant to be triggered."
      )
      Behaviors.stopped

    case _ => Behaviors.unhandled
  }

  /**
    * Handle the responses from source handlers
    *
    * @param stateData          State information
    * @param initializedSources Mapping of all yet initialized sources
    * @param activeSources      Mapping of all sources, that have been initialized
    * @return The specified behavior
    */
  def handleSourceResponses(
      stateData: ExtractorStateData,
      initializedSources: Map[String, ActorRef[SourceHandlerMessage]],
      activeSources: Map[String, ActorRef[SourceHandlerMessage]]
  ): Receive[SupervisorMessage] = Behaviors.receive[SupervisorMessage] {
    case (context, NewUrlsHandled(sourceId)) =>
      context.log
        .debug("Handler for source '{}' reported to have finished.", sourceId)
      val stillActiveSources = activeSources.filterNot {
        case (key, _) => key == sourceId
      }
      if (stillActiveSources.nonEmpty) {
        context.log.debug(
          "Still waiting for the following sources to terminate:\n\t{}",
          stillActiveSources.mkString("\n\t")
        )
        handleSourceResponses(stateData, initializedSources, stillActiveSources)
      } else {
        context.log.info(
          "All sources have reported to have finished. Shut down the distinct tag handler."
        )
        stateData.distinctTagHandler ! Terminate
        awaitTagHandlerTermination
      }
    case _ => Behaviors.unhandled
  }

  def awaitTagHandlerTermination: Receive[SupervisorMessage] =
    Behaviors.receive[SupervisorMessage] {
      case (ctx, DistinctTagHandlerTerminated) =>
        ctx.log.info(
          "Distinct tag handler has terminated. Shut down guardian actor."
        )
        Behaviors.stopped
      case (ctx, unknown) =>
        ctx.log.warn(
          "Received unexpected message while waiting for tag handler termination.\n\t{}",
          unknown
        )
        Behaviors.same
    }

  /**
    * Spawn a [[DistinctTagHandler]] and wait for it's completion message.
    *
    * @param context      Current actor context
    * @param apiUri       Uri for the GraphQL API
    * @param authSecret   Auth token for GraphQL API
    * @return
    */
  private def initializeDistinctTagHandler(
      context: ActorContext[SupervisorMessage],
      apiUri: Uri,
      authSecret: String
  ): ActorRef[DistinctTagHandlerMessage] = {
    /* Set up one tag consolidator, that takes care of concurrency management when creating new tag relations */
    context.log.debug("Spawning a distinct tag handler.")
    val distinctTagHandler =
      context.spawn(DistinctTagHandler(), "DistinctTagHandler")
    distinctTagHandler ! InitializeDistinctTagHandler(
      apiUri,
      authSecret,
      context.self
    )
    context.watchWith(distinctTagHandler, DistinctTagHandlerTerminated)
    distinctTagHandler
  }

  /**
    * Initialize source handler per available source
    *
    * @param context              Current actor context
    * @param distinctTagHandler   Reference to distinct tag handler
    * @param profileDirectoryPath Directory path, where to find page profiles
    * @param apiUri               Uri for the GraphQL API
    * @param authSecret           Auth token for GraphQL API
    * @param reAnalysisInterval   Duration, when an entry shall be re-analysed
    * @param repeatDelay          Amount of time, that a) is reference for rate limit and b) delay time for postponed urls
    * @param chunkSize            Amount of url co-workers and amount of urls to be visited within given time
    * @param sources              Collection of available sources
    * @return Defined behavior
    */
  private def initializeChildren(
      context: ActorContext[SupervisorMessage],
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage],
      profileDirectoryPath: String,
      apiUri: Uri,
      authSecret: String,
      reAnalysisInterval: Duration,
      repeatDelay: Duration,
      chunkSize: Int,
      sources: List[SourceView]
  ): Map[String, ActorRef[SourceHandlerMessage]] = {
    /* Read page profile configs */
    val hostToPageProfile = readPageProfileConfigs(profileDirectoryPath)

    sources.flatMap { source =>
      /* Prepare profile config for that source */
      source.url match {
        case Some(sourceUrl) =>
          val sourceWithProtocol =
            if (!sourceUrl.startsWith("http"))
              "https://" + sourceUrl
            else
              sourceUrl
          hostToPageProfile.find {
            case (hostUrl, _) =>
              hostUrl.contains(new URL(sourceWithProtocol).getHost)
          } match {
            case Some(_ -> pageProfile) =>
              context.log.debug(
                "Spawning an actor for source '{}' ('{}').",
                source.id,
                source.name.getOrElse("")
              )
              val handler =
                context.spawn(
                  SourceHandler(),
                  "SourceHandler_" + source.id
                )
              handler ! InitSourceHandler(
                apiUri,
                pageProfile,
                reAnalysisInterval,
                authSecret,
                chunkSize,
                repeatDelay,
                source,
                distinctTagHandler,
                context.self
              )
              Some(source.id -> handler)
            case None =>
              context.log.error(
                "Unable to determine page profile for source '{}' ('{}'). Cannot handle that source.",
                source.id,
                source.name.getOrElse("")
              )
              None
          }
        case None =>
          context.log.error(
            "Cannot handle the source '{}' ('{}'), as it doesn't contain information about host url.",
            source.id,
            source.name.getOrElse("")
          )
          None
      }
    }.toMap
  }

  final case class InitializingStateData(
      profileDirectoryPath: String,
      apiUri: Uri,
      authSecret: String,
      reAnalysisInterval: Duration,
      repeatDelay: Duration,
      chunkSize: Int,
      distinctTagHandlerRef: ActorRef[DistinctTagHandlerMessage],
      sourcesToInitialize: List[SourceView]
  )

  final case class ExtractorStateData(
      reAnalysisInterval: Duration,
      chunkSize: Int,
      repeatDelay: Duration,
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage]
  )

  /**
    * Read all page profile configs from file
    *
    * @param cfgDirectoryPath Path, where to find the configs
    * @return A Mapping from applicable source url to page profile
    */
  private def readPageProfileConfigs(
      cfgDirectoryPath: String
  ): Map[String, ProfileConfig] = {
    val cfgDirectory = new File(cfgDirectoryPath)
    if (cfgDirectory.exists() && cfgDirectory.isDirectory) {
      cfgDirectory.listFiles
        .filter(_.isFile)
        .map(file => ProfileConfig(ConfigFactory.parseFile(file)))
        .map(profileCfg => profileCfg.profile.hostname -> profileCfg)
        .toMap
    } else {
      Map.empty[String, ProfileConfig]
    }
  }
}
