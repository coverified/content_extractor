/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor

import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder
import com.github.tomakehurst.wiremock.client.WireMock.{
  postRequestedFor,
  urlEqualTo
}
import com.github.tomakehurst.wiremock.matching.{EqualToPattern, RegexPattern}
import info.coverified.extractor.analyzer.BrowserHelper
import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.graphql.{Connector, ExtractorQuery}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import info.coverified.test.scalatest.{GraphQlHelper, MockServerSpec}
import sttp.client3.asynchttpclient.zio.SttpClient
import zio.{RIO, URIO, ZIO}
import zio.console.Console

import java.time.Duration

class ExtractorSpec
    extends MockServerSpec
    with ProfileConfigHelper
    with GraphQlHelper
    with BrowserHelper {
  "Given an extractor" when {
    /* Before all hook doesn't trigger here, but I'd like to reuse the instance of extractor */
    mockServer.start()
    val internalSecret = "dummy_secret"
    val extractor = Extractor(
      apiUri,
      Map.empty[String, ProfileConfig],
      Duration.ofHours(48L),
      internalSecret,
      500
    )
    val commonFileEndings =
      PrivateMethod[List[String]](Symbol("commonFileEndings"))

    "handling new urls" should {
      "send correct url query to GraphQL API" in {
        val queryNewUrls =
          PrivateMethod[URIO[Console with SttpClient, List[SimpleUrlView]]](
            Symbol("queryNewUrls")
          )

        val firstEntries = 250
        evaluateWithHttpClientLayer {
          extractor invokePrivate queryNewUrls(firstEntries)
        }

        val nameNotFilter = (ExtractorQuery invokePrivate commonFileEndings())
          .map(fileEnding => s"""{name_not_contains_i:\\"$fileEnding\\"}""")
          .mkString(",\n")

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(new EqualToPattern(flatPrettifiedQuery(s"""
                |{
                | "query":"query{
                |     allUrls(
                |       where:{
                |         AND:[
                |           $nameNotFilter
                |           ],
                |         lastCrawl:\\"1970-01-01T00:00:00.000Z\\"
                |       },
                |       orderBy:[],
                |       first:$firstEntries,
                |       skip:0){id name source{id name acronym url}}
                |   }",
                | "variables":{}
                |}
                |""".stripMargin)))
          )
        }
      }
    }

    "handling existing urls" should {
      "send correct url query to GraphQL API" in {
        val queryExistingUrls =
          PrivateMethod[URIO[Console with SttpClient, List[SimpleUrlView]]](
            Symbol("queryExistingUrls")
          )

        val firstEntries = 250
        val reAnalysisInterVal = Duration.ofHours(48L)
        evaluateWithHttpClientLayer {
          extractor invokePrivate queryExistingUrls(
            firstEntries,
            reAnalysisInterVal
          )
        }

        val nameNotFilter = (ExtractorQuery invokePrivate commonFileEndings())
          .map(
            fileEnding =>
              s"""\\{name_not_contains_i:\\\\"\\$fileEnding\\\\"\\}"""
          )
          .mkString(",\n")

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(
                new RegexPattern(
                  flatPrettifiedQuery(
                    s"""
                     |\\{
                     | "query":"query\\{
                     |     allUrls\\(
                     |       where:\\{
                     |         AND:\\[
                     |           $nameNotFilter
                     |           ],
                     |         lastCrawl_lte:\\\\"[\\w\\d.\\-:]+\\\\",
                     |         lastCrawl_gt:\\\\"1970-01-01T00:00:00\\.000Z\\\\"
                     |       \\},
                     |       orderBy:\\[\\],
                     |       first:$firstEntries,
                     |       skip:0\\)\\{id name source\\{id name acronym url\\}\\}
                     |   \\}",
                     | "variables":\\{\\}
                     |\\}
                     |""".stripMargin
                  )
                )
              )
          )
        }
      }

      val queryEntriesWithSameHash = PrivateMethod[
        RIO[Console with SttpClient, Option[List[SimpleEntryView[String]]]]
      ](Symbol("queryEntriesWithSameHash"))
      "send correct query, when looking for similar entries" in {
        defineStub("""
                     |{
                     |  "data": {
                     |    "allEntries": [
                     |      {
                     |        "id": "ckr7fgk9d0798fdo8sas1v1us",
                     |        "name": "",
                     |        "hasBeenTagged": false,
                     |        "url": {
                     |          "id": "ckr7fdbuw0218fdo803zdc1hy"
                     |        },
                     |        "tags": [],
                     |        "language": null,
                     |        "content": "",
                     |        "summary": "",
                     |        "date": null,
                     |        "disabled": false
                     |      }
                     |    ]
                     |  }
                     |}
                     |""".stripMargin)

        evaluateWithHttpClientLayer {
          extractor invokePrivate queryEntriesWithSameHash(
            "your_content_hash_here"
          )
        }

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withRequestBody(
                new EqualToPattern(
                  """{"query":"query{allEntries(where:{contentHash:\"your_content_hash_here\",disabled:false},orderBy:[],skip:0){id name content summary url{id} date disabled}}","variables":{}}"""
                )
              )
          )
        }
      }

      "stores an disabled entry, if already one exists" in {
        val buildEntryConsideringExistingEntries = PrivateMethod[
          SelectionBuilder[RootMutation, Option[SimpleEntryView[SimpleUrlView]]]
        ](Symbol("buildEntryConsideringExistingEntries"))
        defineStub("""
                        |{
                        |  "data": {
                        |    "createEntry": {
                        |      "id": "ckr7jno3i1708esoias8lztsz",
                        |      "name": "String",
                        |      "hasBeenTagged": false,
                        |      "url": {
                        |        "id": "ckr7jlfto1541esoia8xjc7n3",
                        |        "name": "bla.foo",
                        |        "source": {
                        |          "id": "ckr7ihygt0062esoid13xod1w",
                        |          "name": "bar",
                        |          "acronym": "baz",
                        |          "url": "bar.baz"
                        |        }
                        |      },
                        |      "tags": [],
                        |      "language": null,
                        |      "content": "Some content",
                        |      "summary": "Some summary",
                        |      "date": "2021-07-20T11:15:00.000Z",
                        |      "nextCrawl": null,
                        |      "updatedAt": "2021-07-17T08:31:03.071Z",
                        |      "profileHash": null,
                        |      "eTag": null,
                        |      "contentHash": "",
                        |      "disabled": false
                        |    }
                        |  }
                        |}
                        |""".stripMargin)

        noException shouldBe thrownBy {
          val maybeExistingEntries = Some(
            List(
              SimpleEntryView(
                id = "ckr7fgk9d0798fdo8sas1v1us",
                name = None,
                content = None,
                summary = None,
                url = None,
                date = None,
                disabled = Some(false)
              )
            )
          )
          val mutation = extractor invokePrivate buildEntryConsideringExistingEntries(
            "urlId",
            "The title",
            Some("This summarizes everything"),
            Some("This contains a lot."),
            Some("2021-07-21T22:00:00Z"),
            "contentHash",
            maybeExistingEntries,
            Duration.ofHours(48L)
          )

          evaluateWithHttpClientLayer {
            Connector.sendRequest(
              mutation
                .toRequest(apiUri)
                .header("x-coverified-internal-auth", internalSecret)
            )
          }

          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(
                new RegexPattern(
                  """\{"query":"mutation\{createEntry\(data:\{name:\\"The title\\",url:\{connect:\{id:\\"urlId\\"}},content:\\"This contains a lot\.\\",summary:\\"This summarizes everything\\",date:\\"2021-07-21T22:00:00Z\\",nextCrawl:\\".+\\",contentHash:\\"contentHash\\",disabled:true}\)\{id name content summary url\{id name source\{id name acronym url}} date disabled}}","variables":\{}}"""
                )
              )
          )
        }
      }

      "stores an enabled entry, if none exists, yet" in {
        val buildEntryConsideringExistingEntries = PrivateMethod[
          SelectionBuilder[RootMutation, Option[SimpleEntryView[SimpleUrlView]]]
        ](Symbol("buildEntryConsideringExistingEntries"))
        defineStub("""
                     |{
                     |  "data": {
                     |    "createEntry": {
                     |      "id": "ckr7jno3i1708esoias8lztsz",
                     |      "name": "String",
                     |      "hasBeenTagged": false,
                     |      "url": {
                     |        "id": "ckr7jlfto1541esoia8xjc7n3",
                     |        "name": "bla.foo",
                     |        "source": {
                     |          "id": "ckr7ihygt0062esoid13xod1w",
                     |          "name": "bar",
                     |          "acronym": "baz",
                     |          "url": "bar.baz"
                     |        }
                     |      },
                     |      "tags": [],
                     |      "language": null,
                     |      "content": "Some content",
                     |      "summary": "Some summary",
                     |      "date": "2021-07-20T11:15:00.000Z",
                     |      "nextCrawl": null,
                     |      "updatedAt": "2021-07-17T08:31:03.071Z",
                     |      "profileHash": null,
                     |      "eTag": null,
                     |      "contentHash": "",
                     |      "disabled": false
                     |    }
                     |  }
                     |}
                     |""".stripMargin)

        noException shouldBe thrownBy {
          val maybeExistingEntries = Some(List.empty[SimpleEntryView[String]])
          val mutation = extractor invokePrivate buildEntryConsideringExistingEntries(
            "urlId",
            "The title",
            Some("This summarizes everything"),
            Some("This contains a lot."),
            Some("2021-07-21T22:00:00Z"),
            "contentHash",
            maybeExistingEntries,
            Duration.ofHours(48L)
          )

          evaluateWithHttpClientLayer {
            Connector.sendRequest(
              mutation
                .toRequest(apiUri)
                .header("x-coverified-internal-auth", internalSecret)
            )
          }

          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(
                new RegexPattern(
                  """\{"query":"mutation\{createEntry\(data:\{name:\\"The title\\",url:\{connect:\{id:\\"urlId\\"}},content:\\"This contains a lot.\\",summary:\\"This summarizes everything\\",date:\\"2021-07-21T22:00:00Z\\",nextCrawl:\\".+\\",contentHash:\\"contentHash\\",disabled:false}\)\{id name content summary url\{id name source\{id name acronym url}} date disabled}}","variables":\{}}"""
                )
              )
          )
        }
      }

      val markAsDisabled = PrivateMethod[
        RIO[Console with SttpClient, Option[SimpleEntryView[SimpleUrlView]]]
      ](Symbol("markAsDisabled"))
      "send correct disabling query to GraphQL" in {
        val id = "some_id"
        defineStub("""
           |{
           |  "data": {
           |    "updateEntry": {
           |      "id": "ckr7jno3i1708esoias8lztsz",
           |      "name": "String",
           |      "hasBeenTagged": false,
           |      "url": {
           |        "id": "ckr7jlfto1541esoia8xjc7n3",
           |        "name": "bla.foo",
           |        "source": {
           |          "id": "ckr7ihygt0062esoid13xod1w",
           |          "name": "bar",
           |          "acronym": "baz",
           |          "url": "bar.baz"
           |        }
           |      },
           |      "tags": [],
           |      "language": null,
           |      "content": "Some content",
           |      "summary": "Some summary",
           |      "date": "2021-07-20T11:15:00.000Z",
           |      "nextCrawl": null,
           |      "updatedAt": "2021-07-17T08:31:03.071Z",
           |      "profileHash": null,
           |      "eTag": null,
           |      "contentHash": "",
           |      "disabled": true
           |    }
           |  }
           |}
           |""".stripMargin)

        evaluateWithHttpClientLayer(extractor invokePrivate markAsDisabled(id))

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(
                new EqualToPattern(
                  s"""{"query":"mutation{updateEntry(id:\\\"$id\\\",data:{disabled:true}){id name content summary url{id name source{id name acronym url}} date disabled}}","variables":{}}""".stripMargin
                )
              )
          )
        }
      }

      val attemptToDisable = PrivateMethod[Option[
        RIO[Console with SttpClient, Option[SimpleEntryView[SimpleUrlView]]]
      ]](Symbol("attemptToDisable"))
      "does not disable anything, if no existing entry is apparent" in {
        val id = "some_id"
        val maybeEntries = Some(List.empty[SimpleEntryView[_]])

        extractor invokePrivate attemptToDisable(maybeEntries) match {
          case None => succeed
          case Some(_) =>
            fail("It isn't expected to get an effect to disable anything.")
        }
      }

      "send correct disabling query to GraphQL, if applicable" in {
        val id = "some_id"
        defineStub("""
                     |{
                     |  "data": {
                     |    "updateEntry": {
                     |      "id": "ckr7jno3i1708esoias8lztsz",
                     |      "name": "String",
                     |      "hasBeenTagged": false,
                     |      "url": {
                     |        "id": "ckr7jlfto1541esoia8xjc7n3",
                     |        "name": "bla.foo",
                     |        "source": {
                     |          "id": "ckr7ihygt0062esoid13xod1w",
                     |          "name": "bar",
                     |          "acronym": "baz",
                     |          "url": "bar.baz"
                     |        }
                     |      },
                     |      "tags": [],
                     |      "language": null,
                     |      "content": "Some content",
                     |      "summary": "Some summary",
                     |      "date": "2021-07-20T11:15:00.000Z",
                     |      "nextCrawl": null,
                     |      "updatedAt": "2021-07-17T08:31:03.071Z",
                     |      "profileHash": null,
                     |      "eTag": null,
                     |      "contentHash": "",
                     |      "disabled": true
                     |    }
                     |  }
                     |}
                     |""".stripMargin)
        val maybeEntries = Some(
          List(
            SimpleEntryView(
              id = id,
              name = None,
              content = None,
              summary = None,
              url = None,
              date = None,
              disabled = Some(false)
            )
          )
        )

        (extractor invokePrivate attemptToDisable(maybeEntries))
          .map(evaluateWithHttpClientLayer(_))

        noException shouldBe thrownBy {
          mockServer.verify(
            postRequestedFor(urlEqualTo("/api/graphql"))
              .withHeader(
                "x-coverified-internal-auth",
                new EqualToPattern(internalSecret, false)
              )
              .withRequestBody(
                new EqualToPattern(
                  s"""{"query":"mutation{updateEntry(id:\\\"$id\\\",data:{disabled:true}){id name content summary url{id name source{id name acronym url}} date disabled}}","variables":{}}""".stripMargin
                )
              )
          )
        }
      }
    }
  }

//  "Given an extractor" when {
//    val extractor = Extractor(
//      Config(
//        "",
//        "",
//        Config.DefaultValues.reAnalysisInterval,
//        "",
//        Config.DefaultValues.chunkSize,
//        Config.DefaultValues.repeatDelay
//      ),
//      Map.empty[String, ProfileConfig]
//    )
//    val coVerifiedView: SimpleUrlView = SimpleUrlView(
//      id = "1",
//      name = Some("https://www.coverified.info"),
//      sourceId = Some("1"),
//      entry = None,
//      hasBeenCrawled = false
//    )
//    val ardView = SimpleUrlView(
//      id = "2",
//      name = Some("https://www.ard.de"),
//      sourceId = Some("2"),
//      entry = None,
//      hasBeenCrawled = false
//    )
//    val validViews: List[SimpleUrlView] = List(
//      coVerifiedView,
//      ardView
//    )
//
//    "gathering all profile configs" should {
//      "return empty map, if pointed to non-existent directory" in {
//        val directoryPath =
//          Seq("somewhere", "over", "the", "rainbow").mkString(File.separator)
//
//        val urlToConfig = Extractor.getAllConfigs(directoryPath)
//        urlToConfig.isEmpty shouldBe true
//      }
//
//      "return empty map, if pointed to a file" in {
//        val tempFile = File.createTempFile("configDir", "")
//        tempFile.deleteOnExit()
//
//        val urlToConfig = Extractor.getAllConfigs(tempFile.getAbsolutePath)
//        urlToConfig.isEmpty shouldBe true
//      }
//
//      val tempDirectoryPath = Files.createTempDirectory("profileConfigDir")
//      val tempDirectory = tempDirectoryPath.toFile
//      tempDirectory.deleteOnExit()
//
//      "return correct mapping from valid file" in {
//        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
//          hostName -> writeTempConfig(tempDirectory, hostName)
//        }
//
//        val urlToConfig = Extractor.getAllConfigs(tempDirectory.getAbsolutePath)
//        urlToConfig.size shouldBe expectedConfigs.size
//        expectedConfigs.foreach {
//          case (hostname, TempConfig(expected, tempConfigFile)) =>
//            urlToConfig.get(hostname) match {
//              case Some(config) =>
//                config shouldBe expected
//                tempConfigFile.delete()
//              case None =>
//                fail(
//                  s"Expect to get config for url '$hostname', but got nothing."
//                )
//            }
//        }
//      }
//
//      "not be bothered by additional directories in there" in {
//        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
//          hostName -> writeTempConfig(tempDirectory, hostName)
//        }
//        val additionalDirectory =
//          Files.createTempDirectory(tempDirectoryPath, "additionalDirectory")
//        additionalDirectory.toFile.deleteOnExit()
//
//        val urlToConfig = Extractor.getAllConfigs(tempDirectory.getAbsolutePath)
//        urlToConfig.size shouldBe expectedConfigs.size
//        expectedConfigs.foreach {
//          case (hostname, TempConfig(expected, tempConfigFile)) =>
//            urlToConfig.get(hostname) match {
//              case Some(config) =>
//                config shouldBe expected
//                tempConfigFile.delete()
//              case None =>
//                fail(
//                  s"Expect to get config for url '$hostname', but got nothing."
//                )
//            }
//        }
//      }
//    }
//
//    "querying all available urls" when {
//      "building the query" should {
//        val buildUrlQuery = PrivateMethod[
//          SelectionBuilder[RootQuery, Option[List[Option[SimpleUrlView]]]]
//        ](Symbol("buildUrlQuery"))
//
//        "return correct GraphQL query" in {
//          val pattern =
//            ("query\\{allUrls\\(where:\\{lastCrawl_lte:\".*\"\\},orderBy:\\[\\],first:\\d+,skip:\\d+\\)\\{id name " +
//              "source\\{id name acronym url\\} lastCrawl\\}\\}").r
//
//          val actualQuery =
//            (extractor invokePrivate buildUrlQuery()).toGraphQL().query
//
//          pattern.matches(actualQuery) shouldBe true
//        }
//      }
//
//      "actually receiving url views" should {
//        val getAllUrlViews = PrivateMethod[
//          URIO[Console with SttpClient, Either[Throwable, List[SimpleUrlView]]]
//        ](Symbol("getAllUrlViews"))
//
//        "returns left exception, if url querying fails" in {
//          val body
//              : Either[CalibanClientError, Some[List[Option[SimpleUrlView]]]] =
//            Left(CommunicationError("What did you say?"))
//          val queryEffect = extractor invokePrivate getAllUrlViews()
//          val responseEffect = SttpStubbing.okayCool(queryEffect, body)
//
//          evaluate(responseEffect) match {
//            case Left(CommunicationError(msg, _)) =>
//              msg shouldBe "What did you say?"
//            case Left(exception) =>
//              fail(s"Querying urls failed with wrong exception.", exception)
//            case Right(_) =>
//              fail("Querying urls was meant to fail, but succeeded.")
//          }
//        }
//
//        "returns an empty list, if none has been sent as response" in {
//          val body: Right[Nothing, Option[List[Option[SimpleUrlView]]]] =
//            Right(None)
//          val queryEffect = extractor invokePrivate getAllUrlViews()
//          val responseEffect = SttpStubbing.okayCool(queryEffect, body)
//
//          evaluate(responseEffect) match {
//            case Right(listOfUrlViews) =>
//              listOfUrlViews.isEmpty shouldBe true
//            case Left(exception) =>
//              fail("Querying urls was meant to succeed, but failed.", exception)
//          }
//        }
//
//        "return an empty list, if no urls are available" in {
//          val body: Either[CalibanClientError, Option[
//            List[Option[SimpleUrlView]]
//          ]] =
//            Right(Some(List.empty[Option[SimpleUrlView]]))
//          val queryEffect = extractor invokePrivate getAllUrlViews()
//          val responseEffect = SttpStubbing.okayCool(queryEffect, body)
//
//          evaluate(responseEffect) match {
//            case Right(listOfUrlViews) =>
//              listOfUrlViews.isEmpty shouldBe true
//            case Left(exception) =>
//              fail("Querying urls was meant to succeed, but failed.", exception)
//          }
//        }
//
//        "return correct views" in {
//          val body: Either[CalibanClientError, Some[List[SimpleUrlView]]] =
//            Right(Some(validViews))
//          val queryEffect = extractor invokePrivate getAllUrlViews()
//          val responseEffect = SttpStubbing.okayCool(queryEffect, body)
//
//          evaluate(responseEffect) match {
//            case Right(listOfUrlViews) =>
//              listOfUrlViews.size shouldBe 2
//              validViews.forall(scheme => listOfUrlViews.contains(scheme)) shouldBe true
//            case Left(exception) =>
//              fail("Querying urls was meant to succeed, but failed.", exception)
//          }
//        }
//      }
//    }
//
//    "acquiring all needed information" should {
//      val acquireNeededInformation = PrivateMethod[
//        ZIO[Console with SttpClient, Throwable, NeededInformation]
//      ](Symbol("acquireNeededInformation"))
//
//      "return empty url list if querying of them fails" in {
//        val body
//            : Either[CalibanClientError, Some[List[Option[SimpleUrlView]]]] =
//          Left(CommunicationError("What did you say?"))
//        val queryEffect = extractor invokePrivate acquireNeededInformation()
//        val responseEffect = SttpStubbing.okayCool(queryEffect, body)
//
//        evaluate(responseEffect) match {
//          case NeededInformation(_, availableUrlViews) =>
//            availableUrlViews.isEmpty shouldBe true
//        }
//      }
//
//      "return all needed information" in {
//        /* Preparation of profile configs */
//        val tempDirectoryPath = Files.createTempDirectory("profileConfigDir")
//        val tempDirectory = tempDirectoryPath.toFile
//        tempDirectory.deleteOnExit()
//        val expectedConfigs = Seq("a", "b", "c").map { hostName =>
//          hostName -> writeTempConfig(tempDirectory, hostName)
//        }
//
//        /* Preparation of url query response */
//        val responseBody
//            : Either[CalibanClientError, Some[List[Some[SimpleUrlView]]]] =
//          Right(Some(validViews.map(Some(_))))
//
//        /* Point Extractor to correct config directory */
//        val extractor =
//          Extractor(
//            Config(
//              "",
//              tempDirectoryPath.toAbsolutePath.toString,
//              Config.DefaultValues.reAnalysisInterval,
//              "",
//              Config.DefaultValues.chunkSize,
//              Config.DefaultValues.repeatDelay
//            ),
//            Map.empty[String, ProfileConfig]
//          )
//
//        /* Build complete effect */
//        val queryEffect = extractor invokePrivate acquireNeededInformation()
//        val responseEffect = SttpStubbing.okayCool(queryEffect, responseBody)
//
//        /* Have a look at the outcome */
//        evaluate(responseEffect) match {
//          case NeededInformation(_, availableUrlViews) =>
//            /* --- Only checking existence, as content is tested in other tests */
//            availableUrlViews.size shouldBe validViews.size
//        }
//      }
//    }
//
//    "filtering for matching config" when {
//      val getProfile4Url =
//        PrivateMethod[Try[ProfileConfig]](Symbol("getProfile4Url"))
//
//      "return none, if no matching config is available" in {
//        Extractor invokePrivate getProfile4Url(
//          "https://www.coverified.info",
//          Map.empty[String, ProfileConfig]
//        ) match {
//          case Failure(exception: ConfigException) =>
//            exception.msg shouldBe "Unable to get config for url 'https://www.coverified.info'."
//          case Failure(exception) =>
//            fail("Failed with wrong exception.", exception)
//          case Success(_) =>
//            fail("Gathering config was meant to fail, but succeeded.")
//        }
//      }
//
//      "return correct profile config" in {
//        val hostnameToConfig = Seq("coverified.info", "ard.de")
//          .map(hostname => hostname -> getConfig(hostname))
//          .toMap
//        val expectedConfig = hostnameToConfig.getOrElse(
//          "coverified.info",
//          fail("Unable to acquire config, I just created")
//        )
//
//        forAll(
//          Table(
//            "url",
//            "https://www.coverified.info/",
//            "https://www.coverified.info/impressum",
//            "https://www.coverified.info/about"
//          )
//        ) { url =>
//          (Extractor invokePrivate getProfile4Url(url, hostnameToConfig)) shouldBe Success(
//            expectedConfig
//          )
//        }
//      }
//    }
//
//    "extracting information" when {
//      val extractInformation =
//        PrivateMethod[Try[RawEntryInformation]](
//          Symbol("extractInformation")
//        )
//      val mockBrowser = MockBrowser(Map(coverifiedUrl -> validUrlPageDoc))
//
//      "fails either given no url or source id" in {
//        forAll(
//          Table(
//            ("url", "sourceId"),
//            (None, None),
//            (Some("url"), None),
//            (None, Some("sourceId"))
//          )
//        ) { (url, sourceId) =>
//          val maliciousUrlView = SimpleUrlView(
//            id = "malicious view",
//            name = url,
//            sourceId = sourceId,
//            entry = None,
//            hasBeenCrawled = false
//          )
//          extractor invokePrivate extractInformation(
//            maliciousUrlView,
//            Map("test" -> getConfig("test")),
//            mockBrowser
//          ) match {
//            case Failure(exception: ExtractionException) =>
//              exception.msg shouldBe s"Unable to extract information, as at least url or source are not known for url view '$maliciousUrlView'."
//            case Failure(exception) =>
//              fail("Test failed with wrong exception.", exception)
//            case Success(_) =>
//              fail(
//                "Extraction of information should fail with incomplete information."
//              )
//          }
//        }
//      }
//
//      "succeeds if given proper information" in {
//        val url = coverifiedUrl
//
//        val urlView = SimpleUrlView(
//          id = "fine view",
//          name = Some(url),
//          sourceId = Some("source id"),
//          entry = None,
//          hasBeenCrawled = false
//        )
//        extractor invokePrivate extractInformation(
//          urlView,
//          Map(url -> getConfig(url)),
//          mockBrowser
//        ) match {
//          case Success(_) => succeed
//          case Failure(exception) =>
//            fail("Extraction of information was not meant to fail.", exception)
//        }
//      }
//
//      "checking against existing" when {
//        val checkAgainstExisting = PrivateMethod[Option[
//          Either[UpdateEntryInformation, CreateEntryInformation]
//        ]](Symbol("checkAgainstExisting"))
//        val rawEntryInformation = RawEntryInformation(
//          Some("title"),
//          Some("Summary"),
//          Some("Content"),
//          Some("date")
//        )
//
//        "having no entry apparent at all, mark for creation" in {
//          extractor invokePrivate checkAgainstExisting(
//            rawEntryInformation,
//            None
//          ) match {
//            case Some(
//                Right(CreateEntryInformation(title, summary, content, date))
//                ) =>
//              title shouldBe rawEntryInformation.title
//              summary shouldBe rawEntryInformation.summary
//              content shouldBe rawEntryInformation.content
//              date shouldBe rawEntryInformation.date
//            case Some(value) =>
//              fail(
//                s"Checking against existing entry failed with wrong value '$value'."
//              )
//            case None =>
//              fail("The entry needs to be created but is marked as un-altered.")
//          }
//        }
//
//        "having same entry in database, mark as nothing to do" in {
//          extractor invokePrivate checkAgainstExisting(
//            rawEntryInformation,
//            Some(
//              SimpleEntryView(
//                "existing_id",
//                rawEntryInformation.title,
//                rawEntryInformation.content,
//                rawEntryInformation.summary,
//                Some(coverifiedUrl),
//                rawEntryInformation.date
//              )
//            )
//          ) match {
//            case None => succeed
//            case Some(value) =>
//              fail(
//                s"Checking against existing entry with same information delivered '$value', although nothing needs to be done."
//              )
//          }
//        }
//
//        "having a different entry in data base, mark for update" in {
//          extractor invokePrivate checkAgainstExisting(
//            rawEntryInformation,
//            Some(
//              SimpleEntryView(
//                "existing_id",
//                Some("url"),
//                rawEntryInformation.content,
//                Some("Different summary"),
//                rawEntryInformation.content,
//                rawEntryInformation.date
//              )
//            )
//          ) match {
//            case Some(
//                Left(UpdateEntryInformation(id, title, summary, content, date))
//                ) =>
//              id shouldBe "existing_id"
//              title shouldBe rawEntryInformation.title
//              summary shouldBe rawEntryInformation.summary
//              content shouldBe rawEntryInformation.content
//              date shouldBe rawEntryInformation.date
//            case Some(value) =>
//              fail(
//                s"Checking against existing entry failed with wrong value '$value'."
//              )
//            case None =>
//              fail("The entry needs to be created but is marked as un-altered.")
//          }
//        }
//      }
//
//      "succeeds in building entry to freshly insert" should {
//        val buildEntry = PrivateMethod[SelectionBuilder[RootMutation, Option[
//          SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
//        ]]](Symbol("buildEntry"))
//        "create an entry correctly" in {
//          val selectionBuilder = Extractor invokePrivate buildEntry(
//            "1",
//            Some("Title"),
//            Some("Summary"),
//            Some("content"),
//            Some("2021-06-09T22:00:00.000Z")
//          )
//
//          selectionBuilder match {
//            case SelectionBuilder.Field(name, _, _, arguments, _) =>
//              name shouldBe "createEntry"
//              arguments.size shouldBe 1
//              arguments.headOption match {
//                case Some(Argument(name, value)) =>
//                  name shouldBe "data"
//                  value match {
//                    case Some(eci: EntryCreateInput) =>
//                      eci.name shouldBe Some("Title")
//                      eci.content shouldBe Some("content")
//                      eci.summary shouldBe Some("Summary")
//                      eci.url shouldBe Some(
//                        UrlRelateToOneInput(
//                          None,
//                          Some(UrlWhereUniqueInput(Some("1"))),
//                          None,
//                          None
//                        )
//                      )
//                      eci.date shouldBe Some("2021-06-09T22:00:00.000Z")
//                      eci.tags shouldBe None
//                      eci.language shouldBe None
//                      eci.hasBeenTagged shouldBe None
//                    case None => fail("Data should actually contain data")
//                  }
//                case None => fail("Expected to get one argument")
//              }
//            case _ => fail("Got wrong result")
//          }
//        }
//      }
//
//      "succeeds in building entry to update" should {
//        val updateEntry = PrivateMethod[SelectionBuilder[RootMutation, Option[
//          SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
//        ]]](Symbol("updateEntry"))
//        "create an entry correctly" in {
//          val selectionBuilder = Extractor invokePrivate updateEntry(
//            "1",
//            Some("Title"),
//            Some("Summary"),
//            Some("content"),
//            Some("2021-06-09T22:00:00.000Z")
//          )
//
//          selectionBuilder match {
//            case SelectionBuilder.Field(name, _, _, arguments, _) =>
//              name shouldBe "updateEntry"
//              arguments.size shouldBe 2
//              arguments.foreach {
//                case Argument("id", value) => value shouldBe "1"
//                case Argument("data", Some(eui: EntryUpdateInput)) =>
//                  eui.name shouldBe Some("Title")
//                  eui.content shouldBe Some("content")
//                  eui.summary shouldBe Some("Summary")
//                  eui.tags shouldBe Some(
//                    TagRelateToManyInput(disconnectAll = Some(true))
//                  )
//                  eui.date shouldBe Some("2021-06-09T22:00:00.000Z")
//                  eui.language shouldBe None
//                  eui.hasBeenTagged shouldBe Some(false)
//                case Argument("data", Some(wrongData)) =>
//                  fail(s"Got wrong data: '$wrongData'")
//                case Argument("data", None) =>
//                  fail("Data should actually contain data")
//              }
//            case _ => fail("Got wrong result")
//          }
//        }
//      }
//    }
//
//    "updating url entries" when {
//      "building the update mutation" should {
//        val buildUrlUpdateMutation =
//          PrivateMethod[SelectionBuilder[RootMutation, Option[SimpleUrlView]]](
//            Symbol("buildUrlUpdateMutation")
//          )
//        "succeed" in {
//          Extractor invokePrivate buildUrlUpdateMutation(
//            "foo",
//            Some(coverifiedUrl),
//            Some("bar")
//          ) match {
//            case Field(name, _, _, arguments, _) =>
//              name shouldBe "updateUrl"
//              arguments.size shouldBe 2
//              arguments.foreach {
//                case Argument("id", value) => value shouldBe "foo"
//                case Argument("data", value) =>
//                  value match {
//                    case Some(UrlUpdateInput(_, _, lastCrawl)) =>
//                      inside(lastCrawl) {
//                        case Some(timeStamp) =>
//                          timeStamp should not be "1970-01-01T00:00:00.000Z"
//                        case malicious =>
//                          fail(
//                            s"Got malicious last crawl time stamp: '$malicious'"
//                          )
//                      }
//                    case _ => fail("Got wrong entry.")
//                  }
//              }
//            case _ => fail("Got wrong mutation.")
//          }
//        }
//      }
//
//      "sending update mutation to API" should {
//        val updateUrlView = PrivateMethod[
//          RIO[Console with SttpClient, Option[SimpleUrlView]]
//        ](Symbol("updateUrlView"))
//
//        "succeed, if API replies okay" in {
//          val updateEffect = extractor invokePrivate updateUrlView(
//            coVerifiedView
//          )
//
//          evaluate(SttpStubbing.postOkay(updateEffect)) match {
//            case Some(
//                SimpleUrlView(id, url, sourceId, entryId, hasBeenCrawled)
//                ) =>
//              id shouldBe coVerifiedView.id
//              url shouldBe coVerifiedView.name
//              sourceId shouldBe sourceId
//              entryId shouldBe None
//              hasBeenCrawled shouldBe false
//            case Some(unexpected) =>
//              fail(s"Passed with unexpected outcome: '$unexpected'.")
//            case None => fail("Updating url entry was meant to succeed.")
//          }
//        }
//      }
//    }
//
//    "inserting entries" when {
//      val storeMutation =
//        PrivateMethod[RIO[Console with SttpClient, Option[
//          SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView]
//        ]]](
//          Symbol("storeMutation")
//        )
//
//      "succeeds, if API replies okay" in {
//        val mutation = Mutation.createEntry(
//          Some(
//            EntryCreateInput(
//              name = Some("Title"),
//              summary = Some("summary"),
//              content = Some("content"),
//              url = Some(
//                UrlRelateToOneInput(
//                  connect =
//                    Some(UrlWhereUniqueInput(id = Some(coverifiedUrlId)))
//                )
//              ),
//              date = Some("2021-06-13T11:20:00.000000Z")
//            )
//          )
//        )(
//          SimpleEntry.view(SimpleUrl.view)
//        )
//
//        evaluate(
//          SttpStubbing.postOkay(extractor invokePrivate storeMutation(mutation))
//        ) match {
//          case Some(SimpleEntryView(_, name, content, summary, url, date)) =>
//            name shouldBe Some("Title")
//            summary shouldBe Some("summary")
//            content shouldBe Some("content")
//            url shouldBe Some(
//              SimpleUrlView(
//                "1",
//                Some("https://coverified.info"),
//                Some("1"),
//                None,
//                hasBeenCrawled = true
//              )
//            )
//            date shouldBe Some("2021-06-13T11:20:00.000000Z")
//          case Some(unexpected) =>
//            fail(s"Passed with unexpected outcome: '$unexpected'.")
//          case None => fail("Updating entries should succeed.")
//        }
//      }
//    }
//
//    "handling scraped information" when {
//      val handleExtractedInformation = PrivateMethod[HandleEntryAndUrlEffect](
//        Symbol("handleExtractedInformation")
//      )
//      val scrapedInformation = RawEntryInformation(
//        Some("title"),
//        Some("Summary"),
//        Some("Content"),
//        Some("date")
//      )
//
//      "having unchanged information, only receive an url update mutation" in {
//        val urlView = SimpleUrlView(
//          "1",
//          Some(coverifiedUrl),
//          Some("1"),
//          Some(
//            SimpleEntryView(
//              "1",
//              Some("title"),
//              Some("Content"),
//              Some("Summary"),
//              Some(coverifiedUrl),
//              Some("date")
//            )
//          ),
//          hasBeenCrawled = true
//        )
//
//        extractor invokePrivate handleExtractedInformation(
//          scrapedInformation,
//          urlView
//        ) match {
//          case (None, Some(_)) => succeed
//          case (Some(_), Some(_)) =>
//            fail(
//              "Received effect to alter entry in data base although that is not expected."
//            )
//          case (_, None) =>
//            fail(
//              "Did not received effect to alter url in data base although that is expected."
//            )
//        }
//      }
//
//      "having information, that are not in data base, receive an mutation to alter entry as well" in {
//        val urlView = SimpleUrlView(
//          "1",
//          Some(coverifiedUrl),
//          Some("1"),
//          None,
//          hasBeenCrawled = false
//        )
//
//        extractor invokePrivate handleExtractedInformation(
//          scrapedInformation,
//          urlView
//        ) match {
//          case (Some(_), Some(_)) => succeed
//          case (None, Some(_)) =>
//            fail(
//              "Received no effect to alter entry in data base although that is expected."
//            )
//          case (_, None) =>
//            fail(
//              "Did not received effect to alter url in data base although that is expected."
//            )
//        }
//      }
//    }
//
//    "handling a single url" when {
//      val handleUrl =
//        PrivateMethod[HandleEntryAndUrlEffect](Symbol("handleUrl"))
//
//      "handing in invalid information" should {
//        "fail" in {
//          extractor invokePrivate handleUrl(
//            coVerifiedView,
//            Map.empty[String, ProfileConfig]
//          ) match {
//            case (None, None) => succeed
//            case _ =>
//              fail(
//                s"Handling an url with invalid accompanying information passed, although was meant to fail."
//              )
//          }
//        }
//      }
//
//      "handing in proper information" should {
//        "pass" in {
//          val hostNameToConfig = Seq(
//            coVerifiedView.name.getOrElse("unknown_url")
//          ).map(hostname => hostname -> getConfig(hostname)).toMap
//          extractor invokePrivate handleUrl(coVerifiedView, hostNameToConfig) match {
//            case (Some(entryEffect), Some(urlEffect)) =>
//              evaluate(SttpStubbing.postOkay(entryEffect)) match {
//                case Some(_: SimpleEntryView[SimpleUrlView]) =>
//                  succeed
//                case None =>
//                  fail(
//                    s"Handling url failed in data base (entry manipulation)."
//                  )
//              }
//              evaluate(SttpStubbing.postOkay(urlEffect)) match {
//                case Some(_: SimpleUrlView) =>
//                  succeed
//                case None =>
//                  fail(s"Handling url failed in data base (url manipulation).")
//              }
//            case (None, None) =>
//              fail(
//                "Handling an url with proper information was meant to succeed."
//              )
//          }
//        }
//      }
//    }
//  }
}
