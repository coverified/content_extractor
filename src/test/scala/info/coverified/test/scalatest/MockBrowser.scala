/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import info.coverified.test.scalatest.MockBrowser.{
  DislikeThatUrlException,
  dislikedUrl
}
import org.jsoup.nodes.Document

case class MockBrowser(urlToResponse: Map[String, Document] = Map.empty) {
  def get(url: String): Document =
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
}

object MockBrowser {
  val dislikedUrl: String = "dontLikeThatUrl"
  val validSelector: String = "validSelector"

  final case class DislikeThatUrlException(
      msg: String = "",
      cause: Throwable = None.orNull
  ) extends Exception(msg, cause)
}
