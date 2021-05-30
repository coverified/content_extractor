/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.{CalibanClientError, SelectionBuilder}
import com.typesafe.config.ConfigFactory
import info.coverified.extractor.Extractor.{NeededInformation, getProfile4Url}
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.config.Config
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.Connector
import info.coverified.graphql.schema.CoVerifiedClientSchema.Entry.EntryView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url.UrlView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  CloudinaryImage_File,
  Entry,
  GeoLocation,
  Language,
  LocationGoogle,
  Query,
  Source,
  Tag,
  Url,
  _QueryMeta
}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import sttp.client3.Request
import sttp.client3.asynchttpclient.zio.{SttpClient, send}
import sttp.model.Uri
import zio.{RIO, UIO, ZIO}
import zio.console.Console

import java.io.File

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 26.02.21
  */
final case class Extractor private (apiUrl: Uri, profileDirectoryPath: String) {

  /**
    * Build the extraction effect. It consists of the following steps:
    * <ol>
    *   <li>Acquire all needed information in parallel</li>
    *   <li>Find applicable config</li>
    *   <li>Issue content extraction</li>
    * </ol>
    *
    * @return
    */
  def buildExtractionEffect(): ZIO[Console with SttpClient, Throwable, Unit] = {
    /* Getting hands on that ZIO stuff - flat mapping by for-comprehension */
    for {
      neededInformation <- acquireNeededInformation
      _ <- ZIO.collectAllPar(
        neededInformation.availableUrlViews.flatMap { urlView =>
          extractInformation(urlView, neededInformation.hostNameToProfileConfig)
            .map { mutation =>
              Connector.sendRequest(mutation.toRequest(apiUrl))
            }
        }
      )
    } yield ()
  }

  /**
    * Get all needed information for content extraction in parallel prior to actual processing
    *
    * @return An effect, that evaluates to [[NeededInformation]]
    */
  private def acquireNeededInformation
      : ZIO[Console with SttpClient, Throwable, NeededInformation] =
    getAllConfigs(profileDirectoryPath).zipWithPar(getAllUrlViews) {
      case (hostnameToConfig, urlViews) =>
        NeededInformation(hostnameToConfig, urlViews)
    }

  /**
    * Acquire all page profiles available in the given directory path.
    *
    * @param cfgDirectoryPath The path, where the files are located
    * @return A map from pages' url to their applicable config
    */
  private def getAllConfigs(
      cfgDirectoryPath: String
  ): UIO[Map[String, ProfileConfig]] = ZIO.effectTotal {
    val cfgDirectory = new File(cfgDirectoryPath)
    if (cfgDirectory.exists() && cfgDirectory.isDirectory) {
      cfgDirectory.listFiles
        .filter(_.isFile)
        .map(file => ProfileConfig(ConfigFactory.parseFile(file)))
        .map(profileCfg => profileCfg.profile.hostname -> profileCfg)
        .toMap
    } else {
      Map.empty[String, ProfileConfig]
    }
  }

  /**
    * Asking the Connector for all available urls + additional information within the data source
    *
    * @return An effect, that evaluates to a list of [[UrlView]]s
    */
  private def getAllUrlViews
      : ZIO[Console with SttpClient, Throwable, List[Extractor.UrlView]] =
    Connector
      .sendRequest {
        val request: Request[Either[CalibanClientError, Option[
          List[Option[Extractor.UrlView]]
        ]], Any] = buildUrlQuery.toRequest(apiUrl)
        request
      }
      .map(_.map(_.flatten).getOrElse(List.empty))

  /**
    * Build up a query to get all urls
    *
    * @return A selection builder with the equivalent query
    */
  private def buildUrlQuery
      : SelectionBuilder[RootQuery, Option[List[Option[Extractor.UrlView]]]] =
    Query.allUrls()(
      Url.view(
        Source.view(
          GeoLocation.view(LocationGoogle.view)
        )
      )
    )

  /**
    * Based on the received url view, try to find matching profile and acquire and view onto the entry, that it forms
    * together
    *
    * @param urlView              View onto the url
    * @param urlToProfileConfigs  Mapping from url to profile config to use
    * @param browser              The browser to be used for content extraction
    * @return An effect, that evaluates to an [[Option]] of [[Extractor.EntryView]]
    */
  private def extractInformation(
      urlView: Extractor.UrlView,
      urlToProfileConfigs: Map[String, ProfileConfig],
      browser: Browser = JsoupBrowser()
  ): Option[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    urlView match {
      case UrlView(_, _, Some(url), Some(source)) =>
        getProfile4Url(url, urlToProfileConfigs).flatMap(
          getMutations(url, source.id, _, browser)
        )
      case _ => None
    }

  /**
    * Issue the analyser and try to mutate the given url together with the config into an entry
    *
    * @param url      Queried url
    * @param sourceId The id of the source
    * @param cfg      [[ProfileConfig]] to use
    * @param browser  The browser to be used for content extraction
    * @return An [[Option]] onto an effect, that evaluates to an [[Option]] onto an [[Extractor.EntryView]]
    */
  private def getMutations(
      url: String,
      sourceId: String,
      cfg: ProfileConfig,
      browser: Browser = JsoupBrowser()
  ): Option[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    Analyzer.run(url, sourceId, cfg, browser).flatten
}

object Extractor {
  /* Defining types as shorthands */
  type UrlView = Url.UrlView[Source.SourceView[
    GeoLocation.GeoLocationView[LocationGoogle.LocationGoogleView]
  ]]
  type EntryView =
    Entry.EntryView[CloudinaryImage_File.CloudinaryImage_FileView, Tag.TagView[
      Language.LanguageView,
      CloudinaryImage_File.CloudinaryImage_FileView
    ], _QueryMeta._QueryMetaView, Language.LanguageView, Source.SourceView[
      GeoLocation.GeoLocationView[LocationGoogle.LocationGoogleView]
    ]]

  def apply(config: Config): Extractor =
    new Extractor(config.apiUri, config.profileDirectoryPath)

  /**
    * Data structure to group all data, that can be loaded in parallel prior to actual information extraction
    *
    * @param hostNameToProfileConfig  Mapping a hostname string to applicable [[ProfileConfig]]
    * @param availableUrlViews        List of all available [[UrlView]]s
    */
  final case class NeededInformation(
      hostNameToProfileConfig: Map[String, ProfileConfig],
      availableUrlViews: List[UrlView]
  )

  /**
    * Given a map of hostnames to configs, find that entry, whose hostname is included within the queried url.
    *
    * @param url              Queried url
    * @param hostNameToConfig Mapping a host name onto it's applicable config
    * @return An [[Option]] onto an applicable config
    */
  private def getProfile4Url(
      url: String,
      hostNameToConfig: Map[String, ProfileConfig]
  ): Option[ProfileConfig] =
    hostNameToConfig
      .find {
        case (hostname, _) =>
          url.contains(hostname)
      }
      .map(_._2)
}
