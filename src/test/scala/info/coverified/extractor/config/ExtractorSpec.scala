/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.config

import caliban.client.CalibanClientError
import caliban.client.CalibanClientError.CommunicationError
import info.coverified.extractor.Extractor
import info.coverified.extractor.Extractor.NeededInformation
import info.coverified.extractor.config.ProfileConfigHelper.TempConfig
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.LocationGoogle.LocationGoogleView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Source.SourceView
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url
import info.coverified.graphql.schema.CoVerifiedClientSchema.Url.UrlView
import info.coverified.test.scalatest.{SttpStubbing, ZioSpec}
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.client3.asynchttpclient.zio.SttpClient
import zio.console.Console
import zio.{UIO, ZIO}

import java.io.File
import java.nio.file.Files

class ExtractorSpec
    extends ZioSpec
    with ProfileConfigHelper
    with TableDrivenPropertyChecks {
  "Given an extractor" when {
    val extractor = Extractor(Config("", ""))
    val validViews = List(
      UrlView(
        _label_ = None,
        id = "CoVerified",
        url = Some("https://www.coverified.info"),
        Some(
          SourceView(
            _label_ = None,
            id = "Test id",
            name = Some("Test name"),
            acronym = Some("T"),
            url = Some("https://www.coverified.info"),
            location = Some(
              LocationGoogleView(
                id = Some("Random place"),
                googlePlaceID = Some("random id"),
                formattedAddress = Some("Here Street 12"),
                lat = Some(7.689946),
                lng = Some(51.534592)
              )
            ),
            description = Some("Some useful description"),
            updatedAt = Some("now"),
            createdAt = Some("now")
          )
        )
      ),
      UrlView(
        _label_ = None,
        id = "ARD",
        url = Some("https://www.ard.de"),
        source = None
      )
    )

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
          hostName -> writeTempConfig(tempDirectory, hostName)
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
          hostName -> writeTempConfig(tempDirectory, hostName)
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

    "querying all available urls" should {
      val getAllUrlViews = PrivateMethod[
        ZIO[Console with SttpClient, Throwable, List[Extractor.UrlView]]
      ](Symbol("getAllUrlViews"))

      "fails, if query execution has failed" in {
        val body: Either[CalibanClientError, Some[
          List[Option[Extractor.UrlView]]
        ]] =
          Left(CommunicationError("What did you say?"))
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        intercept[zio.FiberFailure] {
          evaluate(responseEffect)
        }
      }

      "returns an empty list, of none has been sent as response" in {
        val body: Right[Nothing, Option[List[Option[Extractor.UrlView]]]] =
          Right(None)
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        val listOfUrlViews = evaluate(responseEffect)

        listOfUrlViews.isEmpty shouldBe true
      }

      "return an empty list, if no urls are available" in {
        val body: Either[CalibanClientError, Option[
          List[Option[Extractor.UrlView]]
        ]] =
          Right(Some(List.empty[Option[Extractor.UrlView]]))
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        val listOfUrlViews = evaluate(responseEffect)

        listOfUrlViews.isEmpty shouldBe true
      }

      "return correct views" in {
        val body: Either[CalibanClientError, Some[
          List[Some[Url.UrlView[_ <: SourceView[LocationGoogleView]]]]
        ]] =
          Right(Some(validViews.map(Some(_))))
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        val listOfUrlViews = evaluate(responseEffect)

        listOfUrlViews.size shouldBe 2
        validViews.forall(scheme => listOfUrlViews.contains(scheme)) shouldBe true
      }
    }

    "acquiring all needed information" should {
      val acquireNeededInformation = PrivateMethod[
        ZIO[Console with SttpClient, Throwable, NeededInformation]
      ](Symbol("acquireNeededInformation"))

      "fail, if querying available url fails" in {
        val body: Either[CalibanClientError, Some[
          List[Option[Extractor.UrlView]]
        ]] =
          Left(CommunicationError("What did you say?"))
        val queryEffect = extractor invokePrivate acquireNeededInformation()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        intercept[zio.FiberFailure] {
          evaluate(responseEffect)
        }
      }

      "return all needed information" in {
        /* Preparation of profile configs */
        val tempDirectoryPath = Files.createTempDirectory("profileConfigDir")
        val tempDirectory = tempDirectoryPath.toFile
        tempDirectory.deleteOnExit()
        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
          hostName -> writeTempConfig(tempDirectory, hostName)
        }

        /* Preparation of url query response */
        val responseBody: Either[CalibanClientError, Some[
          List[Some[Url.UrlView[_ <: SourceView[LocationGoogleView]]]]
        ]] =
          Right(Some(validViews.map(Some(_))))

        /* Point Extractor to correct config directory */
        val extractor =
          Extractor(Config("", tempDirectoryPath.toAbsolutePath.toString))

        /* Build complete effect */
        val queryEffect = extractor invokePrivate acquireNeededInformation()
        val responseEffect = SttpStubbing.okayCool(queryEffect, responseBody)

        /* Have a look at the outcome */
        evaluate(responseEffect) match {
          case NeededInformation(hostNameToProfileConfig, availableUrlViews) =>
            /* --- Only checking existence, as content is tested in other tests */
            hostNameToProfileConfig.size shouldBe expectedConfigs.size
            availableUrlViews.size shouldBe validViews.size
        }
      }
    }

    "filtering for matching config" when {
      val getProfile4Url =
        PrivateMethod[Option[ProfileConfig]](Symbol("getProfile4Url"))

      "return none, if no matching config is available" in {
        Extractor invokePrivate getProfile4Url(
          "https://www.coverified.info",
          Map.empty[String, ProfileConfig]
        ) shouldBe None
      }

      "return correct profile config" in {
        val hostnameToConfig = Seq("coverified.info", "ard.de")
          .map(hostname => hostname -> getConfig(hostname))
          .toMap
        val expectedConfig = hostnameToConfig
          .get("coverified.info")
          .orElse(fail("Unable to acquire config, I just created"))

        forAll(
          Table(
            "url",
            "https://www.coverified.info/",
            "https://www.coverified.info/impressum",
            "https://www.coverified.info/about"
          )
        ) { url =>
          (Extractor invokePrivate getProfile4Url(url, hostnameToConfig)) shouldBe expectedConfig
        }
      }
    }
  }
}
