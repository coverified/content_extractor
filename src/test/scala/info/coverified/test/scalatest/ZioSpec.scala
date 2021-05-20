/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import zio.ZIO

trait ZioSpec extends UnitSpec {
  private val runtime = zio.Runtime.default

  /**
    * Evaluate a given effect
    *
    * @param z  The effect to evaluate
    * @tparam E Type of possible error
    * @tparam A Type of outcome
    * @return The outcome
    */
  def evaluate[E, A](z: ZIO[zio.ZEnv, E, A]): A = runtime.unsafeRun(z)
}
