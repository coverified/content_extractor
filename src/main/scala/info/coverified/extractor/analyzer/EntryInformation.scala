/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

/**
  * All information that are needed in order to build an [[info.coverified.graphql.schema.CoVerifiedClientSchema.Entry]]
  *
  * @param title    Title of the page
  * @param summary  Summary of the content
  * @param content  Actual content
  * @param date     Date of the article
  */
final case class EntryInformation(
    title: Option[String],
    summary: Option[String],
    content: Option[String],
    date: Option[String]
)
