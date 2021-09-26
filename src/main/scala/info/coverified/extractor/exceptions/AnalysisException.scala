/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.exceptions

final case class AnalysisException(
    msg: String = "",
    cause: Throwable = None.orNull
) extends Exception(msg, cause)
