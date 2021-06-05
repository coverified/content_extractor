/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url

object SimpleUrl {

  /**
    * Little sister of [[Url]], but without resolving linked information
    *
    * @param id       Identifier
    * @param name     The actual url
    * @param sourceId Id of the source to belong to
    */
  final case class SimpleUrlView(
      id: String,
      name: Option[String],
      sourceId: Option[String]
  )

  def view: SelectionBuilder[Url, SimpleUrlView] =
    (Url.id ~ Url.name ~ Url.source(SimpleSource.view)).map {
      case ((id, name), source) => SimpleUrlView(id, name, source.map(_.id))
    }
}
