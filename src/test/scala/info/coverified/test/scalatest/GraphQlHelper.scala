/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

trait GraphQlHelper {
  def flatPrettifiedQuery(pretty: String): String =
    "(?<=[{(\\[\\])},\"]) +".r
      .replaceAllIn("[\\n\\t]".r.replaceAllIn(pretty, ""), "")
}
