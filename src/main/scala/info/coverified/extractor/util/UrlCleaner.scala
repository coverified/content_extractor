/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.util

import io.lemonlabs.uri.{Url, UrlPath}

import scala.util.matching.Regex

object UrlCleaner {

  private val unwantedQueryParams =
    Vector("nn", "gtp", "imgdownload", "download", "shoppingCart")

  private val regexFilter = Vector(";jsessionid=.*?(?=\\?)|;jsessionid=.*".r)

  def cleanUrl(url: String): String = clean(Url.parse(url)).toStringPunycode

  def mergeHostAndUrl(url: String, host: String): String =
    cleanUrl(
      if (url.contains(host)) {
        url
      } else if (url.startsWith("//")) {
        s"https:$url"
      } else {
        s"$host$url"
      }
    )

  private def clean(url: Url): Url =
    url
      .removeParams(unwantedQueryParams)
      .withFragment(None)
      .toAbsoluteUrl
      .filterRegexes(regexFilter)

  implicit class RichUrl(private val url: Url) extends AnyVal {

    def filterRegexes(regexes: Iterable[Regex]): Url =
      url.withPath(
        UrlPath(regexes.foldLeft(url.path.parts)((urlParts, regex) => {
          filterRegex(urlParts, regex)
        }))
      )

    private def filterRegex(
        urlParts: Vector[String],
        regex: Regex
    ): Vector[String] =
      urlParts.map(regex.replaceAllIn(_, ""))

  }
}
