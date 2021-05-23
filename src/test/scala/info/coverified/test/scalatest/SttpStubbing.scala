/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import caliban.client.CalibanClientError
import info.coverified.graphql.schema.CoVerifiedClientSchema.CloudinaryImage_File.CloudinaryImage_FileView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Entry.EntryView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Language.LanguageView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Tag.TagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  CloudinaryImage_File,
  EntryTypeType,
  GeoLocation,
  Language,
  LocationGoogle,
  Source,
  Tag,
  _QueryMeta
}
import sttp.client3.{
  BasicRequestBody,
  MultipartBody,
  NoBody,
  RequestT,
  Response,
  StreamBody,
  StringBody
}
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

  def okayEntry[E <: Throwable, T](
      queryEffect: ZIO[Console with SttpClient, E, T]
  ): ZIO[zio.ZEnv, Throwable, T] = {
    val stubEffect = for {
      _ <- whenRequestMatchesPartial {
        case RequestT(POST, _, StringBody(queryString, _, _), _, _, _, _) =>
          val responseBody: Right[CalibanClientError, Option[EntryView[
            CloudinaryImage_File.CloudinaryImage_FileView,
            Tag.TagView[
              Language.LanguageView,
              CloudinaryImage_File.CloudinaryImage_FileView
            ],
            _QueryMeta._QueryMetaView,
            Language.LanguageView,
            Source.SourceView[
              GeoLocation.GeoLocationView[LocationGoogle.LocationGoogleView]
            ]
          ]]] = Right(
            Some(
              EntryView(
                _label_ = None,
                id = "a",
                publishDate = "publishDate:\\\\\"(.+)\\\\\",".r
                  .findFirstMatchIn(queryString)
                  .map(_.group(1)),
                title = None,
                subTitle = None,
                image = None,
                content = None,
                summary = None,
                url = "url:\\\\\"(.+)\\\\\",".r
                  .findFirstMatchIn(queryString)
                  .map(_.group(1)),
                tags =
                  List.empty[TagView[LanguageView, CloudinaryImage_FileView]],
                _tagsMeta = None,
                language = None,
                source = None,
                hasBeenTagged = Some(false),
                `type` = Some(EntryTypeType.url),
                updatedAt = None,
                createdAt = None
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
