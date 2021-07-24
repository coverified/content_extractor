/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.MutatorMessage
import info.coverified.extractor.messages.MutatorMessage.{
  CreateEntry,
  InitMutator,
  UpdateUrl
}
import info.coverified.graphql.GraphQLHelper

/**
  * This actor takes care of facing all mutation to the GraphQL API
  */
object Mutator {
  def apply(): Behaviors.Receive[MutatorMessage] = uninitialized

  def uninitialized: Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (_, InitMutator(apiUri, authToken)) =>
        val helper = new GraphQLHelper(apiUri, authToken)
        idle(helper)
      case _ => Behaviors.unhandled
    }

  def idle(helper: GraphQLHelper): Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (context, CreateEntry(createEntryInformation, replyTo)) =>
        context.log.debug(
          "Attempting to create a new entry. Check if there is one with same content."
        )
        /*
         * TODO
         *    1) Calculate content hash
         *    2) Query all entry with given hash and check if this is empty or not.
         *    3) Send mutation with either enabled or disabled entry
         *
         * You may make use of the routines in Extractor ll. 352ff
         * I additionally started implementation of a graph ql helper, that provides means to set up queries / mutations
         * and aids in adding the header etc. Maybe you can add on this as well.
         */
        Behaviors.same
      case (context, UpdateUrl(urlId, replyTo)) =>
        context.log.debug(
          "Attempting to update the url with id '{}'.",
          urlId
        )
        /*
         * TODO
         *    1) Setup and send mutation
         *
         * You may make use of Extractor#buildUrlUpdateMutation (ll. 1267)
         */
        Behaviors.same
      case _ => Behaviors.unhandled
    }
}
