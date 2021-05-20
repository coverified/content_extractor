/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import info.coverified.extractor.Extractor
import info.coverified.extractor.config.ProfileConfigHelper.TempConfig
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.test.scalatest.ZioSpec
import zio.UIO

import java.io.File
import java.nio.file.Files

class ExtractorSpec extends ZioSpec with ProfileConfigHelper {
  "Given an extractor" when {
    val extractor = Extractor(Config("", ""))

    "gathering all profile configs" should {
      val getAllConfigs =
        PrivateMethod[UIO[Map[String, ProfileConfig]]](Symbol("getAllConfigs"))

      "return empty map, if pointed to non-existent directory" in {
        val directoryPath =
          Seq("somewhere", "over", "the", "rainbow").mkString(File.separator)

        val urlToConfig =
          evaluate(extractor invokePrivate getAllConfigs(directoryPath))
        urlToConfig.isEmpty shouldBe true
      }

      "return empty map, if pointed to a file" in {
        val tempFile = File.createTempFile("configDir", "")
        tempFile.deleteOnExit()

        val urlToConfig = evaluate(
          extractor invokePrivate getAllConfigs(tempFile.getAbsolutePath)
        )
        urlToConfig.isEmpty shouldBe true
      }

      val tempDirectoryPath = Files.createTempDirectory("profileConfigDir")
      val tempDirectory = tempDirectoryPath.toFile
      tempDirectory.deleteOnExit()

      "return correct mapping from valid file" in {
        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
          hostName -> createTempConfig(tempDirectory, hostName)
        }

        val urlToConfig = evaluate(
          extractor invokePrivate getAllConfigs(tempDirectory.getAbsolutePath)
        )
        urlToConfig.size shouldBe expectedConfigs.size
        expectedConfigs.foreach {
          case (hostname, TempConfig(expected, tempConfigFile)) =>
            urlToConfig.get(hostname) match {
              case Some(config) =>
                config shouldBe expected
                tempConfigFile.delete()
              case None =>
                fail(
                  s"Expect to get config for url '$hostname', but got nothing."
                )
            }
        }
      }

      "not be bothered by additional directories in there" in {
        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
          hostName -> createTempConfig(tempDirectory, hostName)
        }
        val additionalDirectory =
          Files.createTempDirectory(tempDirectoryPath, "additionalDirectory")
        additionalDirectory.toFile.deleteOnExit()

        val urlToConfig = evaluate(
          extractor invokePrivate getAllConfigs(tempDirectory.getAbsolutePath)
        )
        urlToConfig.size shouldBe expectedConfigs.size
        expectedConfigs.foreach {
          case (hostname, TempConfig(expected, tempConfigFile)) =>
            urlToConfig.get(hostname) match {
              case Some(config) =>
                config shouldBe expected
                tempConfigFile.delete()
              case None =>
                fail(
                  s"Expect to get config for url '$hostname', but got nothing."
                )
            }
        }
      }
    }
  }
}
