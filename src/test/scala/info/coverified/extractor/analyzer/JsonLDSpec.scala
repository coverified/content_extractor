/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import info.coverified.extractor.analyzer.JsonLD.{Logo, Publisher}
import info.coverified.test.scalatest.UnitSpec
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupDocument
import org.jsoup.Jsoup

import java.io.File
import scala.util.Success

class JsonLDSpec extends UnitSpec {

  "A JSON-LD extractor" should {

    "extract json ld from a html page successfully" in {
      val doc = JsoupDocument(
        Jsoup.parse(
          new File("src/test/resources/jsonLD/test.html"),
          "UTF-8"
        )
      )

      JsonLD(doc) shouldBe Success(
        JsonLD(
          Some("http://schema.org"),
          Some("Article"),
          Some("Christian Kuhlicke, Volker Meyer"),
          Some("Nachhaltige Hochwasservorsorge | bpb"),
          Some("2013-08-05T00:00:00+01:00"),
          Some("2013-08-05T12:13:20+01:00"),
          Some("2013-08-05T00:00:00+01:00"),
          Some(
            "https://www.bpb.de/gesellschaft/umwelt/hochwasserschutz/166131/nachhaltige-hochwasservorsorge"
          ),
          Some(
            "Hochwasserereignisse scheinen an Zahl und Intensität zuzunehmen. Mit technischen Schutzmaßnahmen allein lässt sich das Risiko nur verringern. Ist ein vollständiger und nachhaltiger Hochwasserschutz überhaupt möglich? Und wie lässt sich sinnvoll vorso"
          ),
          Some(
            "Hochwasserschutz, Überschwemmungen, Umweltkatastrophe, Hochwasservorsorge"
          ),
          Some(
            Publisher(
              Some("Organization"),
              Some("Bundeszentrale für politische Bildung"),
              Some(
                Logo(
                  Some("ImageObject"),
                  Some(
                    "https://www.bpb.de/sites/all/themes/bpb/images/bpb_logo_ldjson.jpg"
                  ),
                  Some(144),
                  Some(60)
                )
              )
            )
          ),
          Some(
            Logo(
              Some("ImageObject"),
              Some(
                "https://www.bpb.de/cache/images/6/166136-3x2-facebook.jpg?B0E01"
              ),
              Some(900),
              Some(600)
            )
          )
        )
      )
    }

  }

  "A JsonLD parser" should {

    val validJsonLDString =
      """
        |   {
        |            "@context":			"http://schema.org",
        |            "@type":			"Article",
        |            "author":			"Christian Kuhlicke, Volker Meyer",
        |            "headline":			"Nachhaltige Hochwasservorsorge | bpb",
        |            "datePublished":	"2013-08-05T00:00:00+01:00",
        |            "dateCreated":	    "2013-08-05T12:13:20+01:00",
        |            "dateModified":	    "2013-08-05T00:00:00+01:00",
        |            "mainEntityOfPage":	"https:\/\/www.bpb.de\/gesellschaft\/umwelt\/hochwasserschutz\/166131\/nachhaltige-hochwasservorsorge",
        |            "description":      "Hochwasserereignisse scheinen an Zahl und Intensit\u00e4t zuzunehmen. Mit technischen Schutzma\u00dfnahmen allein l\u00e4sst sich das Risiko nur verringern. Ist ein vollst\u00e4ndiger und nachhaltiger Hochwasserschutz \u00fcberhaupt m\u00f6glich? Und wie l\u00e4sst sich sinnvoll vorso",
        |            "keywords":         "Hochwasserschutz, \u00dcberschwemmungen, Umweltkatastrophe, Hochwasservorsorge",
        |            "publisher": {
        |                "@type":	"Organization",
        |                "name":		"Bundeszentrale f\u00fcr politische Bildung",
        |                "logo": {
        |                    "@type":	"ImageObject",
        |                    "url":		"https:\/\/www.bpb.de\/sites\/all\/themes\/bpb\/images\/bpb_logo_ldjson.jpg",
        |                    "width":	"144",
        |                    "height":	"60"
        |                }
        |            }
        |        ,"image" : {
        |            "@type":	"ImageObject",
        |            "url":		"https:\/\/www.bpb.de\/cache\/images\/6\/166136-3x2-facebook.jpg?B0E01",
        |            "width":	"900",
        |            "height":	"600"    }
        |        }
        |""".stripMargin

    "extract publishDate from a valid json ld successfully" in {

      JsonLD
        .decode(validJsonLDString)
        .flatMap(JsonLD.publishDate) shouldBe Success(
        "2013-08-05T00:00:00+01:00"
      )

    }

    "extract creationDate from a valid json ld successfully" in {
      JsonLD
        .decode(validJsonLDString)
        .flatMap(JsonLD.creationDate) shouldBe Success(
        "2013-08-05T12:13:20+01:00"
      )

    }

    "extract modificationDate from a valid json ld successfully" in {
      JsonLD
        .decode(validJsonLDString)
        .flatMap(JsonLD.modificationDate) shouldBe Success(
        "2013-08-05T00:00:00+01:00"
      )

    }

  }

}
