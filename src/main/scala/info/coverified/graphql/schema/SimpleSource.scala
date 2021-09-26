/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.graphql.schema

import caliban.client.SelectionBuilder
import info.coverified.graphql.schema.CoVerifiedClientSchema.{Source, Url}

object SimpleSource {

  /** Little sister of [[Source]], but without resolving linked information
    *
    * @param id
    *   Identifier
    * @param name
    *   Name of source
    * @param acronym
    *   Acronym of the source
    * @param url
    *   Base url
    */
  final case class SimpleSourceView(
      id: String,
      name: Option[String],
      acronym: Option[String],
      url: Option[String]
  )

  def view: SelectionBuilder[Source, SimpleSourceView] =
    (Source.id ~ Source.name ~ Source.acronym ~ Source.url).map {
      case (((id, name), acronym), url) =>
        SimpleSourceView(id, name, acronym, url)
    }
}
