/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import com.typesafe.config.ConfigFactory
import info.coverified.extractor.analyzer.Analyzer
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.Connector
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  Entry,
  GeoLocation,
  LocationGoogle,
  Query,
  Source,
  Url
}
import sttp.client3.asynchttpclient.zio.{SttpClient, send}
import sttp.model.Uri
import zio.ZIO
import zio.console.Console

import java.io.File

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 26.02.21
  */
final case class Extractor(apiUrl: Uri) {

  def getExistingEntries: ZIO[Console with SttpClient, Throwable, List[_]] = ???

  def getAllUrls = {
    // get existent urls
    val urlsQuery = Query.allUrls()(
      Url.view(
        Source.view(
          GeoLocation.view(LocationGoogle.view)
        )
      )
    )
    val existingUrls = Connector
      .sendRequest(urlsQuery.toRequest(apiUrl))
      .map(_.map(_.flatten).getOrElse(List.empty))
    existingUrls
  }

  def getAllConfigs(cfgFolderPath: File): Map[String, ProfileConfig] = {
    if (cfgFolderPath.exists() && cfgFolderPath.isDirectory) {
      cfgFolderPath.listFiles
        .filter(_.isFile)
        .map(file => ProfileConfig(ConfigFactory.parseFile(file)))
        .map(profileCfg => profileCfg.profile.hostname -> profileCfg)
        .toMap
    } else {
      Map.empty
    }
  }

  def getProfile4Url(
      url: String,
      configs: Map[String, ProfileConfig]
  ): Option[ProfileConfig] = {
    configs
      .find {
        case (hostname, cfg) =>
          url.contains(hostname)
      }
      .map(_._2)
  }

  def getMutations(url: String, sourceId: String, cfg: ProfileConfig) = {
    Analyzer
      .run(url, sourceId, cfg)
      .map(mut => Connector.sendRequest(mut.toRequest(apiUrl)))
  }

}
