/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.messages

import sttp.model.Uri

sealed trait MutatorMessage
object MutatorMessage {
  final case class InitMutator(apiUri: Uri, authToken: String)
      extends MutatorMessage
}
