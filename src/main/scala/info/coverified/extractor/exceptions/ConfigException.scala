/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.exceptions

final case class ConfigException(
    msg: String = "",
    cause: Throwable = None.orNull
) extends Exception(msg, cause)
