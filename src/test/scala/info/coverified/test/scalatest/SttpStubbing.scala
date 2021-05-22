/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import sttp.client3.Response
import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import sttp.client3.asynchttpclient.zio.stubbing.whenRequestMatchesPartial
import zio.ZIO
import zio.console.Console

/**
  * Assist in stubbing the [[sttp.client3.SttpBackend]]
  */
object SttpStubbing {

  /**
    * Answer any request with a [[Response#Ok]] and the given body. It prepends an effect, that takes [[SttpStubbing]]
    * as environment to the effect, that contains a query.
    *
    * @param queryEffect  Effect containing a query
    * @param body         Actual payload of the answer
    * @tparam B           Type of the body
    * @tparam E           Error type
    * @tparam T           Result type of query effect
    * @return An effect, that can be evaluated with stubbed sttp backend
    */
  def okayCool[B, E <: Throwable, T](
      queryEffect: ZIO[Console with SttpClient, E, T],
      body: B
  ): ZIO[zio.ZEnv, Throwable, T] = {
    val stubEffect = for {
      _ <- whenRequestMatchesPartial { _ =>
        Response.ok(body)
      }
    } yield ()
    val responseEffect = stubEffect *> queryEffect
    responseEffect.provideCustomLayer(AsyncHttpClientZioBackend.stubLayer)
  }
}
