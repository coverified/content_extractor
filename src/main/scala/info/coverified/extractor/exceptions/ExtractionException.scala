/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.exceptions

final case class ExtractionException(
    msg: String = "",
    cause: Throwable = None.orNull
) extends Exception(msg, cause)
