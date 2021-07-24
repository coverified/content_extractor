/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import info.coverified.extractor.messages.MutatorMessage
import info.coverified.extractor.messages.MutatorMessage.InitMutator
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
      case _ => Behaviors.unhandled
    }
}
