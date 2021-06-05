/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import caliban.client.{Argument, CalibanClientError, SelectionBuilder}
import caliban.client.CalibanClientError.CommunicationError
import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder.Field
import info.coverified.extractor.Extractor.NeededInformation
import info.coverified.extractor.analyzer.BrowserHelper
import info.coverified.extractor.config.ProfileConfigHelper.TempConfig
import info.coverified.extractor.config.{Config, ProfileConfigHelper}
import info.coverified.extractor.exceptions.{
  ConfigException,
  ExtractionException
}
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryCreateInput,
  Mutation,
  UrlRelateToOneInput,
  UrlUpdateInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.{SimpleEntry, SimpleUrl}
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import info.coverified.test.scalatest.{MockBrowser, SttpStubbing, ZioSpec}
import org.scalatest.prop.TableDrivenPropertyChecks
import sttp.client3.asynchttpclient.zio.SttpClient
import zio.console.Console
import zio.{RIO, UIO, ZIO}

import java.io.File
import java.nio.file.Files
import java.time.Duration
import scala.util.{Failure, Success, Try}

class ExtractorSpec
    extends ZioSpec
    with ProfileConfigHelper
    with BrowserHelper
    with TableDrivenPropertyChecks {
  "Given an extractor" when {
    val extractor = Extractor(Config("", "", Duration.ofHours(48L)))
    val coVerifiedView: SimpleUrlView = SimpleUrlView(
      id = "1",
      name = Some("https://www.coverified.info"),
      sourceId = Some("1")
    )
    val ardView = SimpleUrlView(
      id = "2",
      name = Some("https://www.ard.de"),
      sourceId = Some("2")
    )
    val validViews: List[SimpleUrlView] = List(
      coVerifiedView,
      ardView
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
        ZIO[Console with SttpClient, Throwable, List[SimpleUrlView]]
      ](Symbol("getAllUrlViews"))

      "fails, if query execution has failed" in {
        val body
            : Either[CalibanClientError, Some[List[Option[SimpleUrlView]]]] =
          Left(CommunicationError("What did you say?"))
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        intercept[zio.FiberFailure] {
          evaluate(responseEffect)
        }
      }

      "returns an empty list, of none has been sent as response" in {
        val body: Right[Nothing, Option[List[Option[SimpleUrlView]]]] =
          Right(None)
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        val listOfUrlViews = evaluate(responseEffect)

        listOfUrlViews.isEmpty shouldBe true
      }

      "return an empty list, if no urls are available" in {
        val body
            : Either[CalibanClientError, Option[List[Option[SimpleUrlView]]]] =
          Right(Some(List.empty[Option[SimpleUrlView]]))
        val queryEffect = extractor invokePrivate getAllUrlViews()
        val responseEffect = SttpStubbing.okayCool(queryEffect, body)

        val listOfUrlViews = evaluate(responseEffect)

        listOfUrlViews.isEmpty shouldBe true
      }

      "return correct views" in {
        val body: Either[CalibanClientError, Some[List[Some[SimpleUrlView]]]] =
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
        val body
            : Either[CalibanClientError, Some[List[Option[SimpleUrlView]]]] =
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
        val responseBody
            : Either[CalibanClientError, Some[List[Some[SimpleUrlView]]]] =
          Right(Some(validViews.map(Some(_))))

        /* Point Extractor to correct config directory */
        val extractor =
          Extractor(
            Config(
              "",
              tempDirectoryPath.toAbsolutePath.toString,
              Duration.ofHours(48L)
            )
          )

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
        PrivateMethod[Try[ProfileConfig]](Symbol("getProfile4Url"))

      "return none, if no matching config is available" in {
        Extractor invokePrivate getProfile4Url(
          "https://www.coverified.info",
          Map.empty[String, ProfileConfig]
        ) match {
          case Failure(exception: ConfigException) =>
            exception.msg shouldBe "Unable to get config for url 'https://www.coverified.info'."
          case Failure(exception) =>
            fail("Failed with wrong exception.", exception)
          case Success(_) =>
            fail("Gathering config was meant to fail, but succeeded.")
        }
      }

      "return correct profile config" in {
        val hostnameToConfig = Seq("coverified.info", "ard.de")
          .map(hostname => hostname -> getConfig(hostname))
          .toMap
        val expectedConfig = hostnameToConfig.getOrElse(
          "coverified.info",
          fail("Unable to acquire config, I just created")
        )

        forAll(
          Table(
            "url",
            "https://www.coverified.info/",
            "https://www.coverified.info/impressum",
            "https://www.coverified.info/about"
          )
        ) { url =>
          (Extractor invokePrivate getProfile4Url(url, hostnameToConfig)) shouldBe Success(
            expectedConfig
          )
        }
      }
    }

    "extracting information" when {
      val getMutation =
        PrivateMethod[Try[RIO[Console with SttpClient, Option[SimpleUrlView]]]](
          Symbol("getMutation")
        )

      "getting mutation with correct information, it succeeds" in {
        val url = coverifiedUrl
        val urlId = coverifiedUrlId
        val profileConfig = getConfig(coverifiedUrl)
        val mockBrowser = MockBrowser(Map(coverifiedUrl -> validUrlPageDoc))

        extractor invokePrivate getMutation(
          url,
          urlId,
          profileConfig,
          mockBrowser
        ) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Failed with an exception.", exception)
        }
      }

      val extractInformation =
        PrivateMethod[Try[RIO[Console with SttpClient, Option[SimpleUrlView]]]](
          Symbol("extractInformation")
        )
      val mockBrowser = MockBrowser(Map(coverifiedUrl -> validUrlPageDoc))

      "fails either given no url or source id" in {
        forAll(
          Table(
            ("url", "sourceId"),
            (None, None),
            (Some("url"), None),
            (None, Some("sourceId"))
          )
        ) { (url, sourceId) =>
          val maliciousUrlView = SimpleUrlView(
            id = "malicious view",
            name = url,
            sourceId = sourceId
          )
          extractor invokePrivate extractInformation(
            maliciousUrlView,
            Map("test" -> getConfig("test")),
            mockBrowser
          ) match {
            case Failure(exception: ExtractionException) =>
              exception.msg shouldBe s"Unable to extract information, as at least url or source are not known for url view '$maliciousUrlView'."
            case Failure(exception) =>
              fail("Test failed with wrong exception.", exception)
            case Success(_) =>
              fail(
                "Extraction of information should fail with incomplete information."
              )
          }
        }
      }

      "succeeds if given proper information" in {
        val url = coverifiedUrl

        val urlView = SimpleUrlView(
          id = "malicious view",
          name = Some(url),
          sourceId = Some("source id")
        )
        extractor invokePrivate extractInformation(
          urlView,
          Map(url -> getConfig(url)),
          mockBrowser
        ) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Extraction of information was not meant to fail.", exception)
        }
      }
    }

    "updating url entries" when {
      "building the update mutation" should {
        val buildUrlUpdateMutation =
          PrivateMethod[SelectionBuilder[RootMutation, Option[SimpleUrlView]]](
            Symbol("buildUrlUpdateMutation")
          )
        "succeed" in {
          Extractor invokePrivate buildUrlUpdateMutation(
            "foo",
            Some(coverifiedUrl),
            Some("bar")
          ) match {
            case Field(name, _, _, arguments, _) =>
              name shouldBe "updateUrl"
              arguments.size shouldBe 2
              arguments.foreach {
                case Argument("id", value) => value shouldBe "foo"
                case Argument("data", value) =>
                  value match {
                    case Some(UrlUpdateInput(url, _, _, _)) =>
                      url shouldBe Some(coverifiedUrl)
                    case _ => fail("Got wrong entry.")
                  }
              }
            case _ => fail("Got wrong mutation.")
          }
        }
      }

      "sending update mutation to API" should {
        val updateUrlView = PrivateMethod[
          RIO[Console with SttpClient, Option[SimpleUrlView]]
        ](Symbol("updateUrlView"))

        "succeed, if API replies okay" in {
          val updateEffect = extractor invokePrivate updateUrlView(
            coVerifiedView
          )

          evaluate(SttpStubbing.postOkay(updateEffect)) match {
            case Some(SimpleUrlView(id, url, sourceId)) =>
              id shouldBe coVerifiedView.id
              url shouldBe coVerifiedView.name
              sourceId shouldBe sourceId
            case Some(unexpected) =>
              fail(s"Passed with unexpected outcome: '$unexpected'.")
            case None => fail("Updating url entry was meant to succeed.")
          }
        }
      }
    }

    "inserting entries" when {
      val storeMutation =
        PrivateMethod[RIO[Console with SttpClient, Option[
          SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
        ]]](
          Symbol("storeMutation")
        )

      "succeeds, if API replies okay" in {
        val mutation = Mutation.createEntry(
          Some(
            EntryCreateInput(
              name = Some("Title"),
              summary = Some("summary"),
              content = Some("content"),
              url = Some(
                UrlRelateToOneInput(
                  connect = Some(UrlWhereUniqueInput(id = coverifiedUrlId))
                )
              )
            )
          )
        )(
          SimpleEntry.view(SimpleUrl.view)
        )

        evaluate(
          SttpStubbing.postOkay(extractor invokePrivate storeMutation(mutation))
        ) match {
          case Some(SimpleEntryView(_, name, content, summary, url)) =>
            name shouldBe Some("Title")
            summary shouldBe Some("summary")
            content shouldBe Some("content")
            url shouldBe Some(
              SimpleUrlView("1", Some("https://coverified.info"), Some("1"))
            )
          case Some(unexpected) =>
            fail(s"Passed with unexpected outcome: '$unexpected'.")
          case None => fail("Updating entries should succeed.")
        }
      }
    }

    "handling urls" when {
      val handleUrl = PrivateMethod[Option[RIO[
        Console with SttpClient,
        (
            Option[SimpleEntry.SimpleEntryView[SimpleUrlView]],
            Option[SimpleUrlView]
        )
      ]]](Symbol("handleUrl"))

      "handing in invalid information" should {
        "fail" in {
          extractor invokePrivate handleUrl(
            coVerifiedView,
            Map.empty[String, ProfileConfig]
          ) match {
            case None => succeed
            case Some(value) =>
              fail(
                s"Handling an url with invalid accompanying information passed with '$value', although was meant to fail."
              )
          }
        }
      }

      "handing in proper information" should {
        "pass" in {
          val hostNameToConfig = Seq(
            coVerifiedView.name.getOrElse("unknwon_url")
          ).map(hostname => hostname -> getConfig(hostname)).toMap
          extractor invokePrivate handleUrl(coVerifiedView, hostNameToConfig) match {
            case Some(effect) =>
              evaluate(SttpStubbing.postOkay(effect)) match {
                case (
                    Some(_: SimpleEntryView[SimpleUrlView]),
                    Some(_: SimpleUrlView)
                    ) =>
                  succeed
                case (maybeEntryView, maybeUrlView) =>
                  fail(
                    s"Handling url failed with: '$maybeEntryView', '$maybeUrlView'."
                  )
              }
            case None =>
              fail(
                "Handling an url with proper information was meant to succeed."
              )
          }
        }
      }
    }
  }
}
