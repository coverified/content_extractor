/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.analyzer

import com.typesafe.scalalogging.LazyLogging
import info.coverified.extractor.analyzer.JsonLD.{Logo, Publisher}
import info.coverified.extractor.exceptions.AnalysisException
import io.circe
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.sentry.Sentry
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.element
import net.ruippeixotog.scalascraper.dsl.DSL._

import scala.util.{Failure, Success, Try}

final case class JsonLD(
    `@context`: Option[String],
    `@type`: Option[String],
    author: Option[String],
    headline: Option[String],
    datePublished: Option[String],
    dateCreated: Option[String],
    dateModified: Option[String],
    mainEntityOfPage: Option[String],
    description: Option[String],
    keywords: Option[String],
    publisher: Option[Publisher],
    image: Option[Logo]
)

object JsonLD extends LazyLogging {

  private implicit val decodeJsonLD: Decoder[JsonLD] = deriveDecoder[JsonLD]
  private implicit val decodeLogo: Decoder[Logo] = deriveDecoder[Logo]
  private implicit val decodePublisher: Decoder[Publisher] =
    deriveDecoder[Publisher]

  private val ldSelector = "script[type=\"application/ld+json\"]"

  def apply(doc: JsoupDocument): Try[JsonLD] =
    doc >?> element(ldSelector).map(_.innerHtml) match {
      case Some(jsonLD) =>
        decode(jsonLD)
      case None =>
        Failure(AnalysisException("Cannot find JSON-LD in provided document!"))
    }

  def decode(json: String): Try[JsonLD] =
    circe.jawn.decode[JsonLD](json) match {
      case Left(error) =>
        Sentry.captureException(error.fillInStackTrace())
        Failure(error.fillInStackTrace())
      case Right(parsingResult) =>
        Success(parsingResult)
    }

  def publishDate(jsonLD: JsonLD): Try[String] =
    jsonLD.datePublished
      .map(Success(_))
      .getOrElse(
        Failure(
          new NoSuchElementException(
            "Published date is not set in provided documents JSON-LD."
          )
        )
      )

  def publishDate(doc: JsoupDocument): Try[String] =
    apply(doc).flatMap(publishDate)

  def creationDate(jsonLd: JsonLD): Try[String] =
    jsonLd.dateCreated
      .map(Success(_))
      .getOrElse(
        Failure(
          new NoSuchElementException(
            "Creation date is not set in provided documents JSON-LD."
          )
        )
      )

  def creationDate(doc: JsoupDocument): Try[String] =
    apply(doc).flatMap(creationDate)

  def modificationDate(jsonLd: JsonLD): Try[String] =
    jsonLd.dateModified
      .map(Success(_))
      .getOrElse(
        Failure(
          new NoSuchElementException(
            "Modification date is not set in provided documents JSON-LD."
          )
        )
      )

  def modificationDate(doc: JsoupDocument): Try[String] =
    apply(doc).flatMap(modificationDate)

  case class Logo(
      `@type`: Option[String],
      url: Option[String],
      width: Option[Double],
      height: Option[Double]
  )

  case class Publisher(
      `@type`: Option[String],
      name: Option[String],
      logo: Option[Logo]
  )

}
