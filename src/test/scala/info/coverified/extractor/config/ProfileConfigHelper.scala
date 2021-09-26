/**
 * © 2021. CoVerified GmbH
 **/

package info.coverified.extractor.config

import com.typesafe.config.ConfigFactory
import info.coverified.extractor.config.ProfileConfigHelper.TempConfig
import info.coverified.extractor.profile.ProfileConfig

import java.io.{File, PrintWriter}

trait ProfileConfigHelper {

  /** Create a dummy config at a specified location
    *
    * @param directory
    *   Directory, where to write the dummy config to
    * @param hostname
    *   Host name, the config does belong to
    * @return
    *   An instance of [[TempConfig]] that contains the expected config and the
    *   temp file
    */
  def writeTempConfig(directory: File, hostname: String): TempConfig = {
    val (content, expectedConfig) = generateTempConfig(hostname)
    val configFile = File.createTempFile("config", "", directory)
    new PrintWriter(configFile) {
      try {
        write(content)
      } finally {
        close()
      }
    }

    TempConfig(expectedConfig, configFile)
  }

  /** Generates a sample / temporary config for the given host name
    *
    * @param hostname
    *   Host name, the config belongs to
    * @return
    *   A tuple of content and instance
    */
  private def generateTempConfig(hostname: String): (String, ProfileConfig) = {
    val content =
      s"""
         |profile {
         |    hostname = "$hostname"
         |    pageTypes = [{
         |      name = "url"
         |      condition {
         |        path = "$hostname"
         |        selector = "title"
         |    }
         |    selectors {
         |        content = {
         |          selector = "e"
         |        }
         |        title = "h1"
         |        subtitle = "g"
         |        summary = "h"
         |        date = {
         |          tryJsonLdFirst = false
         |          selector = "i"
         |          format = "yyyy-MM-dd'T'HH:mm:ssZ"
         |          attributeVal = None
         |          pattern = None
         |        }
         |        image = "j"
         |        video = "k"
         |        audio = "l"
         |        breadcrumb = "m"
         |        tags = "q"
         |    }
         |    examples = ["n", "o", "p"]
         |    }]
         |}
         |""".stripMargin
    (content, ProfileConfig(ConfigFactory.parseString(content)))
  }

  /** Generates a sample / temporary config for the given host name
    *
    * @param hostname
    *   Host name, the config belongs to
    * @return
    *   The config
    */
  def getConfig(hostname: String): ProfileConfig =
    generateTempConfig(hostname)._2
}

object ProfileConfigHelper {
  final case class TempConfig(expected: ProfileConfig, tempConfigFile: File)
}
