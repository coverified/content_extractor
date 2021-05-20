/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import com.typesafe.config.ConfigFactory
import info.coverified.extractor.config.ProfileConfigHelper.TempConfig
import info.coverified.extractor.profile.ProfileConfig

import java.io.{File, PrintWriter}

trait ProfileConfigHelper {

  /**
    * Create a dummy config at a specified location
    *
    * @param directory  Directory, where to write the dummy config to
    * @param hostname   Host name, the config does belong to
    * @return An instance of [[TempConfig]] that contains the expected config and the temp file
    */
  def createTempConfig(directory: File, hostname: String): TempConfig = {
    val content =
      s"""
        |profile {
        |    hostname = "$hostname"
        |    pageTypes = [{
        |      name = "b"
        |      condition {
        |        path = "c"
        |        selector = "d"
        |    }
        |    selectors {
        |        content = "e"
        |        title = "f"
        |        subtitle = "g"
        |        summary = "h"
        |        date = "i"
        |        image = "j"
        |        video = "k"
        |        audio = "l"
        |        breadcrumb = "m"
        |    }
        |    examples = ["n", "o", "p"]
        |    }]
        |}
        |""".stripMargin
    val expectedConfig = ProfileConfig(ConfigFactory.parseString(content))
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
}

object ProfileConfigHelper {
  final case class TempConfig(expected: ProfileConfig, tempConfigFile: File)
}
