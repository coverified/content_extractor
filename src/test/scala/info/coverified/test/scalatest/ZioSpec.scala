/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.test.scalatest

import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import zio.ZIO
import zio.console.Console

trait ZioSpec extends UnitSpec {
  private val runtime = zio.Runtime.default

  /** Evaluate a given effect
    *
    * @param z
    *   The effect to evaluate
    * @tparam E
    *   Type of possible error
    * @tparam A
    *   Type of outcome
    * @return
    *   The outcome
    */
  def evaluate[E, A](z: ZIO[zio.ZEnv, E, A]): A = runtime.unsafeRun(z)

  /** Evaluate that effect with providing [[AsyncHttpClientZioBackend]] layer
    *
    * @param z
    *   The effect to evaluate
    * @tparam E
    *   Type of possible error
    * @tparam A
    *   Type of outcome
    * @return
    *   The outcome
    */
  def evaluateWithHttpClientLayer[E, A](
      z: ZIO[Console with SttpClient, E, A]
  ): A = evaluate(z.provideCustomLayer(AsyncHttpClientZioBackend.layer()))
}
