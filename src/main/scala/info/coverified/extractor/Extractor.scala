/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import caliban.client.Operations.{RootMutation, RootQuery}
import caliban.client.{CalibanClientError, SelectionBuilder}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.Extractor.{
  EntryView,
  NeededInformation,
  buildUrlUpdateMutation,
  getProfile4Url
}
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.config.Config
import info.coverified.extractor.exceptions.{
  ConfigException,
  ExtractionException
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.Connector
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url.UrlView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  CloudinaryImage_File,
  Entry,
  GeoLocation,
  Language,
  LocationGoogle,
  Mutation,
  Query,
  Source,
  SourceRelateToOneInput,
  SourceWhereUniqueInput,
  Tag,
  Url,
  UrlUpdateInput,
  _QueryMeta
}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import sttp.client3.Request
import sttp.client3.asynchttpclient.zio.SttpClient
import sttp.model.Uri
import zio.{RIO, UIO, ZIO}
import zio.console.Console

import java.io.File
import scala.util.{Failure, Success, Try}

/**
  * Extracting the content from available urls and post the content to data base
  *
  * @version 0.1
  * @since 26.02.21
  */
final case class Extractor private (apiUrl: Uri, profileDirectoryPath: String)
    extends LazyLogging {

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
      NeededInformation(hostNameToProfileConfig, urlViews) <- acquireNeededInformation
      _ <- ZIO.collectAllPar {
        urlViews.flatMap(handleUrl(_, hostNameToProfileConfig))
      }
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
        buildUrlQuery.toRequest(apiUrl)
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
    * Handle the received url. At first, needed information are extracted and then, parallely, entry as well as updated
    * mutation are sent to API.
    *
    * @param urlView              View onto the url
    * @param hostToProfileConfig  Mapping from host name to it's profile config
    * @return [[Option]] onto an effect, that might be put to API
    */
  def handleUrl(
      urlView: Extractor.UrlView,
      hostToProfileConfig: Map[String, ProfileConfig]
  ): Option[
    RIO[Console with SttpClient, (Option[EntryView], Option[Extractor.UrlView])]
  ] =
    extractInformation(urlView, hostToProfileConfig) match {
      case Success(mutation) =>
        val storeMutationEffect = storeMutation(mutation)
        val urlUpdateEffect = updateUrlView(urlView)
        Some(storeMutationEffect.zipPar(urlUpdateEffect))
      case Failure(exception) =>
        logger
          .warn("Analysis of url '{}' failed.", urlView.url, exception)
        None
    }

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
  ): Try[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    urlView match {
      case UrlView(_, _, Some(url), Some(source)) =>
        getProfile4Url(url, urlToProfileConfigs).flatMap(
          getMutation(url, source.id, _, browser)
        )
      case _ =>
        Failure(
          ExtractionException(
            s"Unable to extract information, as at least url or source are not known for url view '$urlView'."
          )
        )
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
  private def getMutation(
      url: String,
      sourceId: String,
      cfg: ProfileConfig,
      browser: Browser = JsoupBrowser()
  ): Try[SelectionBuilder[RootMutation, Option[Extractor.EntryView]]] =
    Analyzer.run(url, sourceId, cfg, browser)

  /**
    * Announce the derived view to API
    *
    * @param mutation Mutation to denote the content
    * @return The equivalent effect
    */
  private def storeMutation(
      mutation: SelectionBuilder[RootMutation, Option[EntryView]]
  ): RIO[Console with SttpClient, Option[EntryView]] =
    Connector.sendRequest(mutation.toRequest(apiUrl))

  /**
    * Build a mutation and send it to API in order to update the url entry
    *
    * @param view The original view to update
    * @return An equivalent effect
    */
  private def updateUrlView(
      view: Extractor.UrlView
  ): RIO[Console with SttpClient, Option[Extractor.UrlView]] = view match {
    case UrlView(_, id, Some(url), Some(source)) =>
      val mutation = buildUrlUpdateMutation(id, url, source.id)
      Connector.sendRequest(mutation.toRequest(apiUrl))
  }
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
  ): Try[ProfileConfig] =
    hostNameToConfig
      .find {
        case (hostname, _) =>
          url.contains(hostname)
      }
      .fold[Try[ProfileConfig]] {
        Failure(ConfigException(s"Unable to get config for url '$url'."))
      } { case (_, config) => Success(config) }

  /**
    * Build the mutation, that is used for updating the url entry in database
    *
    * @param id       Id of entry
    * @param url      Url
    * @param sourceId Id of source
    * @return The needed mutation
    */
  private def buildUrlUpdateMutation(
      id: String,
      url: String,
      sourceId: String
  ): SelectionBuilder[RootMutation, Option[Extractor.UrlView]] =
    Mutation.updateUrl(
      id,
      Some(
        UrlUpdateInput(
          url = Some(url),
          source = Some(
            SourceRelateToOneInput(
              connect = Some(
                SourceWhereUniqueInput(
                  sourceId
                )
              )
            )
          )
        )
      )
    )(
      Url.view(
        Source.view(
          GeoLocation.view(LocationGoogle.view)
        )
      )
    )
}
