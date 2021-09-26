/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.test.scalatest

trait GraphQlHelper {
  def flatPrettifiedQuery(pretty: String): String =
    "(?<=[{(\\[\\])},\"]) +".r
      .replaceAllIn("[\\n\\t]".r.replaceAllIn(pretty, ""), "")
}
