/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.test.scalatest

import info.coverified.test.scalatest.MockBrowser.{
  DislikeThatUrlException,
  MockDoc,
  dislikedUrl
}
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.model.{
  Document,
  Element,
  ElementQuery,
  Node
}

import java.io.{File, InputStream}

class MockBrowser extends Browser {
  override type DocumentType = MockDoc

  override def userAgent: String =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def get(url: String): MockDoc =
    if (url == dislikedUrl)
      throw DislikeThatUrlException("I don't like that url.")
    else
      throw new NotImplementedError(
        "That feature is not implemented in mocked browser."
      )

  override def post(url: String, form: Map[String, String]): MockDoc =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseFile(file: File, charset: String): MockDoc =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseInputStream(
      inputStream: InputStream,
      charset: String
  ): MockDoc =
    throw new NotImplementedError(
      "That feature is not implemented in mocked browser."
    )

  override def parseString(html: String): MockDoc =
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

  case class MockDoc() extends Document {
    override type ElementType = MockElement

    override def location: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked document."
      )

    override def root: MockElement = new MockElement

    override def toHtml: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked document."
      )
  }

  class MockElement extends Element {
    override type ThisType = MockElement

    override def tagName: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def parent: Option[MockElement] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def children: Iterable[MockElement] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def childNodes: Iterable[Node] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def siblings: Iterable[MockElement] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def siblingNodes: Iterable[Node] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def attrs: Map[String, String] =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def hasAttr(name: String): Boolean =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def attr(name: String): String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def text: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def innerHtml: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def outerHtml: String =
      throw new NotImplementedError(
        "That feature is not implemented in mocked element."
      )

    override def select(query: String): ElementQuery[MockElement] = {
      if (query == validSelector)
        ElementQuery(this)
      else
        throw new NotImplementedError(
          "That feature is not implemented in mocked element."
        )
    }
  }

  final case class DislikeThatUrlException(
      msg: String = "",
      cause: Throwable = None.orNull
  ) extends Exception(msg, cause)
}
