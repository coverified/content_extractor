/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor

import akka.actor.typed.ActorSystem
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.actor.ExtractionSupervisor
import info.coverified.extractor.config.Config
import info.coverified.extractor.messages.SupervisorMessage
import info.coverified.extractor.messages.SupervisorMessage.InitSupervisor

import scala.util.{Failure, Success}

object RunWithAkka extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting extraction")

    /* Try to gather config from CLI args or environment variables */
    val config = Config.fromEnv() match {
      case Success(cfg) => cfg
      case Failure(exception) =>
        logger.info(
          "Cannot obtain config from environment variables. Trying to parse from CLI arguments. Cause:",
          exception
        )
        ArgsParser
          .parse(args)
          .flatMap(Config.fromArgs(_).toOption) match {
          case Some(cfg) => cfg
          case None =>
            throw new RuntimeException(
              "Unable to obtain config from CLI arguments."
            )
        }
    }

    logger.debug(
      "The extractor configuration is: {}",
      config.copy(authSecret = "*****")
    )

    /* Start the actor system and forward the config information */
    val actorSystem = ActorSystem[SupervisorMessage](
      ExtractionSupervisor(),
      "ExtractionSupervisor"
    )
    actorSystem ! InitSupervisor(config)
  }
}
