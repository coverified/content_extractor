/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

/**
  * Gathers all information, that can be extracted from a typical video page
  *
  * @param title        Title
  * @param subTitle     Sub title
  * @param summary      Summary of page
  * @param content      Content
  * @param publishDate  Date of publication
  * @param breadCrumbs  Bread crumbs
  * @param videoSrc     Source of image
  */
final case class VideoViewInformation(
    override val title: Option[String],
    override val subTitle: Option[String],
    override val summary: Option[String],
    override val content: Option[String],
    override val publishDate: Option[String],
    override val breadCrumbs: Option[String],
    videoSrc: Option[String]
) extends PageViewInformation
