/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

/**
  * Gathers all information, that can be extracted from a typical url page
  *
  * @param title        Title
  * @param subTitle     Sub title
  * @param content      Content
  * @param publishDate  Date of publication
  */
final case class UrlViewInformation(
    override val title: Option[String],
    override val subTitle: Option[String],
    override val content: Option[String],
    override val publishDate: Option[String]
) extends PageViewInformation
