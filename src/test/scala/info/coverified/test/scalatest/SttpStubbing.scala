/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import caliban.client.CalibanClientError
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.{SimpleUrlView, entryId}
import sttp.client3.{RequestT, Response, StringBody}
import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import sttp.client3.asynchttpclient.zio.stubbing.whenRequestMatchesPartial
import sttp.model.Method.POST
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
      _ <- whenRequestMatchesPartial(_ => Response.ok(body))
    } yield ()
    val responseEffect = stubEffect *> queryEffect
    responseEffect.provideCustomLayer(AsyncHttpClientZioBackend.stubLayer)
  }

  def postOkay[E <: Throwable, T](
      queryEffect: ZIO[Console with SttpClient, E, T]
  ): ZIO[zio.ZEnv, Throwable, T] = {
    val stubEffect = for {
      _ <- whenRequestMatchesPartial {
        case RequestT(POST, _, StringBody(queryString, _, _), _, _, _, _)
            if queryString.contains("mutation{createEntry") =>
          val responseBody: Right[CalibanClientError, Option[
            SimpleEntryView[SimpleUrlView]
          ]] = Right(
            Some(
              SimpleEntryView(
                id = "a",
                name = "name:\\\\\"([^\"]*)\\\\\"".r
                  .findFirstMatchIn(queryString)
                  .map(_.group(1)),
                summary = "summary:\\\\\"([^\"]*)\\\\\"".r
                  .findFirstMatchIn(queryString)
                  .map(_.group(1)),
                content = "content:\\\\\"([^\"]*)\\\\\"".r
                  .findFirstMatchIn(queryString)
                  .map(_.group(1)),
                url = {
                  if (queryString.contains("url:{connect:{id:\\\"1\\\"}}"))
                    Some(
                      SimpleUrlView(
                        id = "1",
                        name = Some("https://coverified.info"),
                        sourceId = Some("1"),
                        entryId = None,
                        hasBeenCrawled = true
                      )
                    )
                  else None
                }
              )
            )
          )
          Response.ok(responseBody)
        case RequestT(POST, _, StringBody(queryString, _, _), _, _, _, _)
            if queryString.contains("mutation{updateUrl") =>
          val responseBody: Right[CalibanClientError, Option[SimpleUrlView]] =
            Right(
              Some(
                SimpleUrlView(
                  id = "id:\\\\\"([^\"]*)\\\\\"".r
                    .findFirstMatchIn(queryString)
                    .map(_.group(1))
                    .getOrElse("ID_NOT_FOUND"),
                  name = Some("https://www.coverified.info"),
                  sourceId = Some("1"),
                  entryId = None,
                  hasBeenCrawled = "lastCrawl:\\\\\"([^\"]*)\\\\\"".r
                    .findFirstMatchIn(queryString)
                    .map(_.group(1))
                    .contains("1970-01-01T00:00:00.000Z")
                )
              )
            )
          Response.ok(responseBody)
      }
    } yield ()
    val responseEffect = stubEffect *> queryEffect
    responseEffect.provideCustomLayer(AsyncHttpClientZioBackend.stubLayer)
  }
}
