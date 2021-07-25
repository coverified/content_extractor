/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import sttp.model.Uri

sealed trait DistinctTagHandlerMessage
object DistinctTagHandlerMessage {
  final case class InitializeDistinctTagHandler(apiUri: Uri, authSecret: String)
      extends DistinctTagHandlerMessage
  final case class ConsolidateArticleTags(contentHash: Long, tags: List[String])
      extends DistinctTagHandlerMessage
  case object Terminate extends DistinctTagHandlerMessage
}
