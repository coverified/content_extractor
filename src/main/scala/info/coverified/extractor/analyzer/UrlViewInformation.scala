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
  * @param summary      Summary of page
  * @param content      Content
  * @param publishDate  Date of publication
  * @param breadCrumbs  Bread crumbs
  * @param imageSrc     Source of image
  */
final case class UrlViewInformation(
    override val title: Option[String],
    override val subTitle: Option[String],
    override val summary: Option[String],
    override val content: Option[String],
    override val publishDate: Option[String],
    override val breadCrumbs: Option[String],
    imageSrc: Option[String]
) extends PageViewInformation
