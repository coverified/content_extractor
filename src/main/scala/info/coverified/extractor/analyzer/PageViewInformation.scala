/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

/**
  * Abstract class describing shared information about pages
  */
abstract class PageViewInformation {
  protected val title: Option[String]
  protected val subTitle: Option[String]
  protected val summary: Option[String]
  protected val content: Option[String]
  protected val publishDate: Option[String]
  protected val breadCrumbs: Option[String]
}
