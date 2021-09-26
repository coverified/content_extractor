/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.DistinctTagHandlerMessage
import info.coverified.extractor.messages.DistinctTagHandlerMessage.{
  ConsolidateArticleTags,
  InitializeDistinctTagHandler,
  Terminate
}
import info.coverified.extractor.messages.MutatorMessage.ConnectToTags
import info.coverified.extractor.messages.SupervisorMessage.DistinctTagHandlerInitialized
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTagCreateInput,
  ArticleTagsCreateInput
}

/**
  * Service actor to ensure a consistent set of article tags
  */
object DistinctTagHandler {
  def apply(): Behaviors.Receive[DistinctTagHandlerMessage] = uninitialized

  def uninitialized: Behaviors.Receive[DistinctTagHandlerMessage] =
    Behaviors.receive {
      case (
          ctx,
          InitializeDistinctTagHandler(apiUri, authSecret, supervisor)
          ) =>
        ctx.log.info("Initializing tag harmonizer.")
        val graphQLHelper = new GraphQLHelper(apiUri, authSecret)
        supervisor ! DistinctTagHandlerInitialized
        idle(graphQLHelper)
      case _ => Behaviors.unhandled
    }

  def idle(
      graphQLHelper: GraphQLHelper
  ): Behaviors.Receive[DistinctTagHandlerMessage] =
    Behaviors.receive[DistinctTagHandlerMessage] {
      case (ctx, ConsolidateArticleTags(contentHash, tags, mutator)) =>
        ctx.log.debug("Attempting to harmonize given tags.")
        /* Check the existing tags */
        val existingTags =
          graphQLHelper.matchingTags(tags).getOrElse(List.empty)
        val existingTagIds = existingTags.map(_.id)
        ctx.log.debug("Found {} yet existing article tags.", existingTags.size)

        /* Create the missing ones */
        val (_, tagsToCreate) = tags.partition { candidateTag =>
          existingTags.exists { tagView =>
            tagView.name.contains(candidateTag)
          }
        }

        /* Create tags and make the best of it. */
        val newTagIds = graphQLHelper.saveArticleTags(tagsToCreate)
        if (newTagIds.size != tagsToCreate.size) {
          ctx.log.warn(
            "Attempted to save {} new article tags and got {} back. Connect them anyway.",
            tagsToCreate.size,
            newTagIds.size
          )
        } else
          ctx.log.debug(
            "Created {} new article tags. Additionally connect to the {} existing ones.",
            newTagIds.size,
            existingTagIds.size
          )
        mutator ! ConnectToTags(contentHash, existingTagIds ++ newTagIds)

        /* No need to change here */
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
