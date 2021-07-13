/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

/**
  * All information that are needed in order to build an [[info.coverified.graphql.schema.CoVerifiedClientSchema.Entry]]
  */
sealed trait EntryInformation {
  protected val title: String
  protected val summary: Option[String]
  protected val content: Option[String]
  protected val date: Option[String]
}

object EntryInformation {

  /**
    * Group all information, that are scraped from site
    *
    * @param title    Title of the page
    * @param summary  Summary of the content
    * @param content  Actual content
    * @param date     Date of the article
    */
  final case class RawEntryInformation(
      override val title: String,
      override val summary: Option[String],
      override val content: Option[String],
      override val date: Option[String]
  ) extends EntryInformation

  /**
    * Group all information to update an existing entry
    *
    * @param id       Identifier of the entry to update
    * @param title    Title of the page
    * @param summary  Summary of the content
    * @param content  Actual content
    * @param date     Date of the article
    */
  final case class UpdateEntryInformation(
      id: String,
      override val title: String,
      override val summary: Option[String],
      override val content: Option[String],
      override val date: Option[String]
  ) extends EntryInformation

  object UpdateEntryInformation {
    def apply(id: String, raw: RawEntryInformation): UpdateEntryInformation =
      new UpdateEntryInformation(
        id,
        raw.title,
        raw.summary,
        raw.content,
        raw.date
      )
  }

  /**
    * Group all information to create a new entry
    *
    * @param title    Title of the page
    * @param summary  Summary of the content
    * @param content  Actual content
    * @param date     Date of the article
    */
  final case class CreateEntryInformation(
      override protected val title: String,
      override protected val summary: Option[String],
      override protected val content: Option[String],
      override protected val date: Option[String]
  ) extends EntryInformation

  object CreateEntryInformation {
    def apply(raw: RawEntryInformation): CreateEntryInformation =
      new CreateEntryInformation(raw.title, raw.summary, raw.content, raw.date)
  }
}
