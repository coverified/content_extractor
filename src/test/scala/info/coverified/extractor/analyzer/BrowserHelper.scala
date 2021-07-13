/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

trait BrowserHelper {
  val coverifiedUrl = "https://www.coverified.info"
  val coverifiedUrlId = "1"

  val validUrlPageDoc: Document = Jsoup.parse(
    """
      |<!DOCTYPE html>
      |
      |<head>
      |    <title id="title">Url page with all information available</title>
      |</head>
      |
      |<body>
      |    <h1>This is an url page with all information available</h1>
      |    <h2 id="subtitle">... with a subtitle</h2>
      |    <span id="breadcrumb">Some bread crumbs</span>
      |    <span id="publishedAt">2021-06-03T13:37:00Z</span>
      |    <p id="summary">This is a summary</p>
      |
      |    <p id="content">
      |      And with all the content.
      |
      |      <img src="find/me/here" id="pic"/>
      |    </p>
      |</body>
      |""".stripMargin
  )

  val validUrlPageDocWithoutOptionalInformation: Document = Jsoup.parse(
    """
      |<!DOCTYPE html>
      |
      |<head>
      |    <title id="title">Url page with all information available</title>
      |</head>
      |
      |<body>
      |    <h1>This is an url page with all information available</h1>
      |</body>
      |""".stripMargin
  )

  val urlPageDocWithoutTitle: Document = Jsoup.parse(
    """
      |<!DOCTYPE html>
      |
      |<head>
      |    <title id="title">Url page with all information available</title>
      |</head>
      |
      |<body>
      |    <h2 id="subtitle">... with a subtitle</h2>
      |    <span id="breadcrumb">Some bread crumbs</span>
      |    <span id="publishedAt">2021-06-03T13:37:00Z</span>
      |    <p id="summary">This is a summary</p>
      |
      |    <p id="content">
      |      And with all the content.
      |
      |      <img src="find/me/here" id="pic"/>
      |    </p>
      |</body>
      |""".stripMargin
  )

  val validVideoPageDoc: Document = Jsoup.parse(
    """
        |<!DOCTYPE html>
        |
        |<head>
        |    <title id="title">Url page with all information available</title>
        |</head>
        |
        |<body>
        |    <h1>This is an url page with all information available</h1>
        |    <h2 id="subtitle">... with a subtitle</h2>
        |    <span id="breadcrumb">Some bread crumbs</span>
        |    <span id="publishedAt">2021-06-03T13:37:00Z</span>
        |    <p id="summary">This is a summary</p>
        |
        |    <p id="content">
        |      And with all the content.
        |
        |     <video id="vid" width="320" height="240" controls>
        |       <source src="find/me/here" type=video/mp4>
        |     </video>
        |    </p>
        |</body>
        |""".stripMargin
  )

  val validVideoPageDocWithoutOptionalInformation: Document = Jsoup.parse(
    """
      |<!DOCTYPE html>
      |
      |<head>
      |    <title id="title">Url page with all information available</title>
      |</head>
      |
      |<body>
      |    <h1>This is an url page with all information available</h1>
      |</body>
      |""".stripMargin
  )
}

object BrowserHelper {
  implicit class RichDocument(doc: Document) {
    def toScraperDoc: JsoupDocument = JsoupDocument(doc)
  }
}
