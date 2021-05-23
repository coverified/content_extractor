/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import info.coverified.test.scalatest.MockBrowser.{
  DislikeThatUrlException,
  dislikedUrl
}
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.model.Document

import java.io.{File, InputStream}

case class MockBrowser(urlToResponse: Map[String, Document] = Map.empty)
    extends Browser {
  override type DocumentType = Document

  override def userAgent: String =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def get(url: String): Document =
    if (url == dislikedUrl)
      throw DislikeThatUrlException("I don't like that url.")
    else {
      urlToResponse.getOrElse(
        url,
        throw new NotImplementedError(
          "That feature is not implemented in mocked browser."
        )
      )
    }

  override def post(url: String, form: Map[String, String]): Document =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseFile(file: File, charset: String): Document =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseInputStream(
      inputStream: InputStream,
      charset: String
  ): Document =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseString(html: String): Document =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def cookies(url: String): Map[String, String] =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def clearCookies(): Unit =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )
}

object MockBrowser {
  val dislikedUrl: String = "dontLikeThatUrl"
  val validSelector: String = "validSelector"

  final case class DislikeThatUrlException(
      msg: String = "",
      cause: Throwable = None.orNull
  ) extends Exception(msg, cause)
}
