/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.config.ConfigFactory
import info.coverified.extractor.Extractor.NeededInformation
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.config.Config
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.Connector
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
        neededInformation.availableUrlViews.flatMap(
          urlViewToEntryView(_, neededInformation.hostNameToProfileConfig)
        )
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
      : ZIO[Console with SttpClient, Throwable, List[Extractor.UrlView]] = {
    val urlsQuery = Query.allUrls()(
      Url.view(
        Source.view(
          GeoLocation.view(LocationGoogle.view)
        )
      )
    )
    Connector
      .sendRequest(urlsQuery.toRequest(apiUrl))
      .map(_.map(_.flatten).getOrElse(List.empty))
  }

  /**
    * Based on the received url view, try to find matching profile and acquire and view onto the entry, that it forms
    * together
    *
    * @param urlView              View onto the url
    * @param urlToProfileConfigs  Mapping from url to profile config to use
    * @return An effect, that evaluates to an [[Option]] of [[Extractor.EntryView]]
    */
  private def urlViewToEntryView(
      urlView: Extractor.UrlView,
      urlToProfileConfigs: Map[String, ProfileConfig]
  ): Option[RIO[Console with SttpClient, Option[Extractor.EntryView]]] =
    urlView match {
      case UrlView(_, _, Some(url), Some(source)) =>
        getProfile4Url(url, urlToProfileConfigs).flatMap(
          profileCfg => getMutations(url, source.id, profileCfg)
        )
      case _ => None
    }

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

  /**
    * Issue the analyser and try to mutate the given url together with the config into an entry
    *
    * @param url      Queried url
    * @param sourceId The id of the source
    * @param cfg      [[ProfileConfig]] to use
    * @return An [[Option]] onto an effect, that evaluates to an [[Option]] onto an [[Extractor.EntryView]]
    */
  def getMutations(
      url: String,
      sourceId: String,
      cfg: ProfileConfig
  ): Option[RIO[Console with SttpClient, Option[Extractor.EntryView]]] = {
    Analyzer
      .run(url, sourceId, cfg)
      .flatMap(_.map(mut => Connector.sendRequest(mut.toRequest(apiUrl))))
  }

  def getExistingEntries: ZIO[Console with SttpClient, Throwable, List[_]] = ???

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
}
