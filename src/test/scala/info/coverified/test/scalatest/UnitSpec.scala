/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpecLike

/**
  * Custom trait grouping commonly used scalatest traits to use for unit tests
  */
trait UnitSpec
    extends Matchers
    with AnyWordSpecLike
    with PrivateMethodTester
    with TableDrivenPropertyChecks