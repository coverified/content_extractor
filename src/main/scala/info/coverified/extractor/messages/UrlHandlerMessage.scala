/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.messages

import akka.actor.typed.ActorRef
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import sttp.model.Uri

import java.time.{Duration, ZoneId}

sealed trait UrlHandlerMessage
object UrlHandlerMessage {

  /** Initialization information for the url handling actor
    *
    * @param mutator
    *   Reference to the mutator
    * @param userAgent
    *   User agent to be sent when reaching out to websites
    * @param browseTimeout
    *   Time out, when reaching out for websites
    * @param targetDateTimePattern
    *   The target date time pattern, in which date time information shall be
    *   sent to [[info.coverified.extractor.actor.Mutator]]
    * @param targetTimeZone
    *   The target time zone, in which date time information shall be sent to
    *   [[info.coverified.extractor.actor.Mutator]]
    * @param apiUri
    *   Location of GraphQL API
    * @param authSecret
    *   Authentication token for GraphQL API
    */
  final case class InitUrlHandler(
      mutator: ActorRef[MutatorMessage],
      userAgent: String,
      browseTimeout: Duration,
      targetDateTimePattern: String,
      targetTimeZone: ZoneId,
      apiUri: Uri,
      authSecret: String
  ) extends UrlHandlerMessage

  final case class HandleNewUrl(
      url: String,
      urlId: String,
      pageProfile: ProfileConfig,
      replyToSourceHandler: ActorRef[SourceHandlerMessage]
  ) extends UrlHandlerMessage

  final case class HandleExistingUrl(
      url: String,
      urlId: String,
      maybeEntry: Option[SimpleEntryView[SimpleUrlView, ArticleTagView]],
      pageProfile: ProfileConfig,
      replyToSourceHandler: ActorRef[SourceHandlerMessage]
  ) extends UrlHandlerMessage

  object Terminate extends UrlHandlerMessage
}
