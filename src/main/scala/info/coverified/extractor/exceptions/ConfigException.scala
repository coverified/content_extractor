/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.exceptions

final case class ConfigException(
    msg: String = "",
    cause: Throwable = None.orNull
) extends Exception(msg, cause)
