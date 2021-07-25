/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors._
import com.typesafe.config.ConfigFactory
import info.coverified.extractor.messages.DistinctTagHandlerMessage.{
  InitializeDistinctTagHandler,
  Terminate
}
import info.coverified.extractor.messages.SourceHandlerMessage.{
  InitSourceHandler,
  Run
}
import info.coverified.extractor.messages.{
  DistinctTagHandlerMessage,
  SourceHandlerMessage,
  SupervisorMessage
}
import info.coverified.extractor.messages.SupervisorMessage.{
  DistinctTagHandlerTerminated,
  InitSupervisor,
  SourceHandled,
  SourceHandlerInitialized
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.GraphQLHelper

import java.io.File
import java.net.URL
import java.time.Duration

object ExtractionSupervisor {
  def apply(): Behavior[SupervisorMessage] = uninitialized

  /**
    * Start uninitialized
    */
  def uninitialized: Receive[SupervisorMessage] = receive[SupervisorMessage] {
    case (
        context,
        msg @ InitSupervisor(
          apiUri,
          profileDirectoryPath,
          reAnalysisInterval,
          authSecret,
          chunkSize,
          repeatDelay
        )
        ) =>
      context.log.info("Received a init message: {}.", msg)

      /* Read page profile configs */
      val hostToPageProfile = readPageProfileConfigs(profileDirectoryPath)

      /* Query all sources */
      val graphQLHelper = new GraphQLHelper(apiUri, authSecret)
      val maybeSources = graphQLHelper.queryAllSources
      graphQLHelper.close()
      maybeSources match {
        case Some(emptySources) if emptySources.isEmpty =>
          context.log.info("There are no sources available. I'm done!")
          Behaviors.stopped
        case Some(sources) =>
          context.log.info(
            "Received {} sources. Spawn an actor for each of them.",
            sources.size
          )

          /* Set up one tag consolidator, that takes care of concurrency management when creating new tag relations */
          context.log.debug("Spawning a distinct tag handler.")
          val distinctTagHandler =
            context.spawn(DistinctTagHandler(), "DistinctTagHandler")
          distinctTagHandler ! InitializeDistinctTagHandler(apiUri, authSecret)
          context.watchWith(distinctTagHandler, DistinctTagHandlerTerminated)
          // TODO: Forward to mutators

          val initializedSources = sources.flatMap { source =>
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

          if (initializedSources.isEmpty) {
            context.log.warn("No source handler has been started. Exit.")
            Behaviors.stopped
          } else {
            val stateData = ExtractorStateData(
              hostToPageProfile,
              reAnalysisInterval,
              chunkSize,
              repeatDelay,
              distinctTagHandler
            )
            handleSourceResponses(stateData, initializedSources, Map.empty)
          }
        case None =>
          context.log.warn(
            "Querying sources did not return a sensible reply. Shut down."
          )
          Behaviors.stopped
      }
    case _ => unhandled
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
    case (context, SourceHandlerInitialized(sourceId, replyTo)) =>
      context.log.debug(
        "Source handler for source '{}' successfully initialized. Trigger it to start action!",
        sourceId
      )
      val stillToBeActivedSourceHandlers = initializedSources.filterNot {
        case (key, _) => key == sourceId
      }
      if (stillToBeActivedSourceHandlers.nonEmpty)
        context.log.debug(
          "Still initializing source handler for sources:\n\t{}",
          stillToBeActivedSourceHandlers.mkString("\n\t")
        )
      else
        context.log.debug("All source handlers initialized.")
      val newActiveSource = activeSources + (sourceId -> replyTo)
      replyTo ! Run(context.self)
      handleSourceResponses(
        stateData,
        stillToBeActivedSourceHandlers,
        newActiveSource
      )
    case (context, SourceHandled(sourceId)) =>
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

  final case class ExtractorStateData(
      hostNameToPageProfile: Map[String, ProfileConfig],
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
