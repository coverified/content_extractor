/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import sttp.model.Uri

import java.time.{Duration, ZoneId}

/**
  * All messages that are understood by the [[info.coverified.extractor.actor.SourceHandler]]
  */
sealed trait SourceHandlerMessage
object SourceHandlerMessage {
  final case class InitSourceHandler(
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
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage],
      replyTo: ActorRef[SupervisorMessage]
  ) extends SourceHandlerMessage

  object MutatorInitialized extends SourceHandlerMessage

  final case class HandleNewUrls(replyTo: ActorRef[SupervisorMessage])
      extends SourceHandlerMessage
  final case class HandleExistingUrls(replyTo: ActorRef[SupervisorMessage])
      extends SourceHandlerMessage

  /**
    * Get report about _single_ handled urls from [[UrlHandlerMessage]]
    */
  sealed trait UrlHandledMessage extends SourceHandlerMessage
  final case class UrlHandledSuccessfully(url: String) extends UrlHandledMessage
  final case class UrlHandledWithFailure(
      url: String,
      urlId: String,
      failure: Throwable
  ) extends UrlHandledMessage

  final case class ScheduleUrl[P](id: String, url: String, payLoad: Option[P])
      extends SourceHandlerMessage

  final case object MutationsCompleted extends SourceHandlerMessage

  object Terminate extends SourceHandlerMessage
}
