/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.DistinctTagHandlerMessage
import info.coverified.extractor.messages.DistinctTagHandlerMessage.{
  ConsolidateArticleTags,
  InitializeDistinctTagHandler,
  Terminate
}
import info.coverified.graphql.GraphQLHelper

/**
  * Service actor to ensure a consistent set of article tags
  */
object DistinctTagHandler {
  def apply(): Behaviors.Receive[DistinctTagHandlerMessage] = uninitialized

  def uninitialized: Behaviors.Receive[DistinctTagHandlerMessage] =
    Behaviors.receive[DistinctTagHandlerMessage] {
      case (ctx, InitializeDistinctTagHandler(apiUri, authSecret)) =>
        ctx.log.info("Initializing tag harmonizer.")
        idle(new GraphQLHelper(apiUri, authSecret))
      case _ => Behaviors.unhandled
    }

  def idle(
      graphQLHelper: GraphQLHelper
  ): Behaviors.Receive[DistinctTagHandlerMessage] =
    Behaviors.receive[DistinctTagHandlerMessage] {
      case (ctx, ConsolidateArticleTags(tags)) =>
        ctx.log.debug("Attempting to harmonize given tags.")
        // TODO: Add logic
        Behaviors.same
      case (ctx, Terminate) =>
        ctx.log.info("Shutting down tag harmonizer.")
        Behaviors.stopped { () =>
          ctx.log.debug("Closing GraphQL resources.")
          graphQLHelper.close()
        }
      case _ => Behaviors.unhandled
    }
}
