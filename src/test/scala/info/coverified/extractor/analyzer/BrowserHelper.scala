/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Document

trait BrowserHelper {
  val coverifiedUrl = "https://wwww.coverified.info"

  val validUrlPageDoc: Document = JsoupBrowser
    .apply()
    .parseString(
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
      |
      |    <p id="content">
      |      And with all the content.
      |    </p>
      |</body>
      |""".stripMargin
    )

  val validUrlPageDocWithoutOptionalInformation: Document = JsoupBrowser
    .apply()
    .parseString(
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

  val validVideoPageDoc: Document = JsoupBrowser
    .apply()
    .parseString(
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
      |
      |    <p id="content">
      |      And with all the content.
      |    </p>
        |</body>
        |""".stripMargin
    )

  val validVideoPageDocWithoutOptionalInformation: Document = JsoupBrowser
    .apply()
    .parseString(
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
