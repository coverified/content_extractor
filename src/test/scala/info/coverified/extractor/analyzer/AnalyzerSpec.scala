/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import caliban.client.Operations.RootMutation
import caliban.client.{Argument, SelectionBuilder}
import info.coverified.extractor.Extractor
import info.coverified.extractor.config.ProfileConfigHelper
import info.coverified.extractor.exceptions.AnalysisException
import info.coverified.extractor.profile.ProfileConfig
import info.coverified.extractor.profile.ProfileConfig.PageType.{
  Condition,
  Selectors
}
import info.coverified.extractor.profile.ProfileConfig.Profile
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  CloudinaryImage_File,
  Entry,
  EntryCreateInput,
  EntryTypeType,
  GeoLocation,
  Language,
  LocationGoogle,
  Source,
  Tag,
  _QueryMeta
}
import info.coverified.test.scalatest.MockBrowser.DislikeThatUrlException
import info.coverified.test.scalatest.{MockBrowser, ZioSpec}
import net.ruippeixotog.scalascraper.model.Document
import org.mockito.scalatest.MockitoSugar
import org.scalatest.Inside.inside

import scala.util.{Failure, Success, Try}

class AnalyzerSpec
    extends ZioSpec
    with ProfileConfigHelper
    with BrowserHelper
    with MockitoSugar {
  "Given an analyzer" when {
    val validPageType = ProfileConfig.PageType(
      condition = Condition(
        path = Some(coverifiedUrl + "/impressum"),
        selector = None
      ),
      examples = List("a", "b"),
      name = "url",
      selectors = Selectors(
        audio = None,
        breadcrumb = Some("#breadcrumb"),
        content = "#content",
        date = Some("#publishedAt"),
        image = Some("#pic"),
        subtitle = Some("#subtitle"),
        summary = Some("#summary"),
        title = "#title",
        video = Some("#vid")
      )
    )

    "determining the page type" should {
      val selectorMatches = PrivateMethod[Boolean](Symbol("selectorMatches"))

      "refuse matching selector, if selector does not lead to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("this won't work"))
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe false
      }

      "confirm matching selector, if selector leads to entries" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = Some("title"))
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      "confirm matching selector, if no selector is set" in {
        val pageTypeWithoutSelectors = validPageType.copy(
          condition = validPageType.condition
            .copy(selector = None)
        )
        Analyzer invokePrivate selectorMatches(
          validUrlPageDoc,
          pageTypeWithoutSelectors
        ) shouldBe true
      }

      val pathMatches = PrivateMethod[Boolean](Symbol("pathMatches"))

      "deny matching path, if path does not match" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = Some("https://wwww.ard.de"))
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe false
      }

      "confirm matching path, if path actually matches" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = Some(coverifiedUrl + "/impressum"))
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      "confirm matching path, if path is not set" in {
        val pageTypeWithPath = validPageType.copy(
          condition = validPageType.condition
            .copy(path = None)
        )
        Analyzer invokePrivate pathMatches(
          coverifiedUrl + "/impressum/subpage",
          pageTypeWithPath
        ) shouldBe true
      }

      val determinePageType =
        PrivateMethod[Try[(String, Selectors)]](Symbol("determinePageType"))
      val validPath = Some(coverifiedUrl + "/impressum")

      "fail, if one of either conditions is not satisfied" in {
        forAll(
          Table(
            ("selector", "path"),
            (Some("some invalid selector"), Some("some invalid path")),
            (Some(MockBrowser.validSelector), Some("some invalid path")),
            (Some("some invalid selector"), validPath)
          )
        ) { (selector, path) =>
          val pageType = validPageType.copy(
            condition =
              validPageType.condition.copy(path = path, selector = selector)
          )

          val profileConfig = ProfileConfig(
            Profile(coverifiedUrl, List(pageType))
          )

          Analyzer invokePrivate determinePageType(
            coverifiedUrl + "/impressum/subpage",
            validUrlPageDoc,
            profileConfig
          ) match {
            case Failure(exception: AnalysisException) =>
              exception.msg shouldBe s"Unable to gather profile config for url '${coverifiedUrl + "/impressum/subpage"}'."
            case Failure(exception) =>
              fail("Failed with wrong exception.", exception)
            case Success(_) =>
              fail("Determination of page type was meant to fail.")
          }
        }
      }

      "succeed, if both conditions are satisfied" in {
        forAll(
          Table(
            ("selector", "path"),
            (None, None),
            (Some("title"), None),
            (None, validPath)
          )
        ) { (selector, path) =>
          val pageType = validPageType.copy(
            condition =
              validPageType.condition.copy(path = path, selector = selector)
          )

          val profileConfig = ProfileConfig(
            Profile(coverifiedUrl, List(pageType))
          )

          Analyzer invokePrivate determinePageType(
            coverifiedUrl + "/impressum/subpage",
            validUrlPageDoc,
            profileConfig
          ) match {
            case Success((name, selectors)) =>
              name shouldBe validPageType.name
              selectors shouldBe validPageType.selectors
            case Failure(exception) =>
              fail(
                "Page type should be determined. Failed with exception.",
                exception
              )
          }
        }
      }
    }

    "building the entries with extracted information" should {
      val buildEntry =
        PrivateMethod[Try[
          SelectionBuilder[RootMutation, Option[Extractor.EntryView]]
        ]](Symbol("buildEntry"))

      "fail on attempt to extract information from unsupported page type" in {
        val url = coverifiedUrl
        val pageType = "unicornPageType"
        val pageDoc = mock[Document]
        val sourceId = "Some Source"
        val selectors = validPageType.selectors

        Analyzer invokePrivate buildEntry(
          url,
          pageDoc,
          pageType,
          sourceId,
          selectors
        ) match {
          case Failure(exception: AnalysisException) =>
            exception.msg shouldBe s"Unknown page type: $pageType"
          case Failure(exception) =>
            fail("Failed with wrong exception.", exception)
          case Success(_) => fail("Building of entry was meant to fail.")
        }
      }

      // TODO CK: Test delegation to correct method

      val extractUrlViewInformation =
        PrivateMethod[UrlViewInformation](Symbol("extractUrlViewInformation"))
      "extract information correctly from url page" in {
        inside(
          Analyzer invokePrivate extractUrlViewInformation(
            validUrlPageDoc,
            validPageType.selectors
          )
        ) {
          case UrlViewInformation(
              title,
              subTitle,
              summary,
              content,
              publishDate,
              breadCrumbs,
              imageSrc
              ) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe Some("... with a subtitle")
            summary.getOrElse(fail("Expected to get a summary.")) shouldBe "This is a summary"
            content shouldBe Some("And with all the content.")
            publishDate.getOrElse(fail("Expected to get a date.")) shouldBe "2021-06-03T13:37:00[UTC]"
            breadCrumbs.getOrElse(fail("Expected to get bread crumbs.")) shouldBe "Some bread crumbs"
            imageSrc.getOrElse(fail("Expected to get an image source.")) shouldBe "find/me/here"
        }
      }

      "extract information correctly from url page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractUrlViewInformation(
            validUrlPageDocWithoutOptionalInformation,
            validPageType.selectors
          )
        ) {
          case UrlViewInformation(
              title,
              subTitle,
              summary,
              content,
              publishDate,
              breadCrumbs,
              imageSrc
              ) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe None
            content shouldBe None
            publishDate shouldBe None
            breadCrumbs shouldBe None
            imageSrc shouldBe None
        }
      }

      val extractVideoViewInformation =
        PrivateMethod[VideoViewInformation](
          Symbol("extractVideoViewInformation")
        )
      "extract information correctly from video page" in {
        inside(
          Analyzer invokePrivate extractVideoViewInformation(
            validVideoPageDoc,
            validPageType.selectors
          )
        ) {
          case VideoViewInformation(
              title,
              subTitle,
              summary,
              content,
              publishDate,
              breadCrumbs,
              videoSrc
              ) =>
            title shouldBe Some("Url page with all information available")
            summary.getOrElse(fail("Expected to get a summary.")) shouldBe "This is a summary"
            content shouldBe Some("And with all the content.")
            publishDate.getOrElse(fail("Expected to get a date.")) shouldBe "2021-06-03T13:37:00[UTC]"
            breadCrumbs.getOrElse(fail("Expected to get bread crumbs.")) shouldBe "Some bread crumbs"
            videoSrc.getOrElse(fail("Expected to get an video source.")) shouldBe "find/me/here"
        }
      }

      "extract information correctly from video page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractVideoViewInformation(
            validVideoPageDocWithoutOptionalInformation,
            validPageType.selectors
          )
        ) {
          case VideoViewInformation(
              title,
              subTitle,
              summary,
              content,
              publishDate,
              breadCrumbs,
              videoSrc
              ) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe None
            content shouldBe None
            publishDate shouldBe None
            breadCrumbs shouldBe None
            videoSrc shouldBe None
        }
      }

      val createUrlEntry =
        PrivateMethod[SelectionBuilder[RootMutation, Option[Entry.EntryView[
          CloudinaryImage_File.CloudinaryImage_FileView,
          Tag.TagView[
            Language.LanguageView,
            CloudinaryImage_File.CloudinaryImage_FileView
          ],
          _QueryMeta._QueryMetaView,
          Language.LanguageView,
          Source.SourceView[
            GeoLocation.GeoLocationView[LocationGoogle.LocationGoogleView]
          ]
        ]]]](Symbol("createUrlEntry"))
      "creates url entry correctly" in {
        val selectionBuilder = Analyzer invokePrivate createUrlEntry(
          coverifiedUrl,
          "Coverified",
          Some("Title"),
          Some("Subtitle"),
          Some("Summary"),
          Some("content"),
          Some("2020-05-23T11:00:00Z"),
          Some("Some bread crumbs"),
          Some("img/source/path")
        )

        selectionBuilder match {
          case SelectionBuilder.Field(name, _, _, arguments, _) =>
            name shouldBe "createEntry"
            arguments.size shouldBe 1
            arguments.headOption match {
              case Some(Argument(name, value)) =>
                name shouldBe "data"
                value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.publishDate shouldBe Some("2020-05-23T11:00:00Z")
                    eci.title shouldBe Some("Title")
                    eci.subTitle shouldBe Some("Subtitle")
                    eci.image shouldBe Some("img/source/path")
                    eci.content shouldBe Some("content")
                    eci.summary shouldBe Some("Summary")
                    eci.url shouldBe Some(coverifiedUrl)
                    eci.tags shouldBe None
                    eci.language shouldBe None
                    eci.source match {
                      case Some(_) => succeed
                      case None =>
                        fail("Expected to get some source information")
                    }
                    eci.hasBeenTagged shouldBe None
                    eci.`type` shouldBe Some(EntryTypeType.url)
                  case None => fail("Data should actually contain data")
                }
              case None => fail("Expected to get one argument")
            }
          case _ => fail("Got wrong result")
        }
      }

      val createVideoEntry =
        PrivateMethod[SelectionBuilder[RootMutation, Option[Entry.EntryView[
          CloudinaryImage_File.CloudinaryImage_FileView,
          Tag.TagView[
            Language.LanguageView,
            CloudinaryImage_File.CloudinaryImage_FileView
          ],
          _QueryMeta._QueryMetaView,
          Language.LanguageView,
          Source.SourceView[
            GeoLocation.GeoLocationView[LocationGoogle.LocationGoogleView]
          ]
        ]]]](Symbol("createVideoEntry"))
      "creates video entry correctly" in {
        val selectionBuilder = Analyzer invokePrivate createVideoEntry(
          coverifiedUrl,
          "Coverified",
          Some("Title"),
          Some("Subtitle"),
          Some("Summary"),
          Some("content"),
          Some("2020-05-23T11:00:00Z"),
          Some("Some bread crumbs"),
          Some("vid/source/path")
        )

        selectionBuilder match {
          case SelectionBuilder.Field(name, _, _, arguments, _) =>
            name shouldBe "createEntry"
            arguments.size shouldBe 1
            arguments.headOption match {
              case Some(Argument(name, value)) =>
                name shouldBe "data"
                value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.publishDate shouldBe Some("2020-05-23T11:00:00Z")
                    eci.title shouldBe Some("Title")
                    eci.subTitle shouldBe Some("Subtitle")
                    eci.image shouldBe Some("vid/source/path")
                    eci.content shouldBe Some("content")
                    eci.summary shouldBe Some("Summary")
                    eci.url shouldBe Some(coverifiedUrl)
                    eci.tags shouldBe None
                    eci.language shouldBe None
                    eci.source match {
                      case Some(_) => succeed
                      case None =>
                        fail("Expected to get some source information")
                    }
                    eci.hasBeenTagged shouldBe None
                    eci.`type` shouldBe Some(EntryTypeType.video)
                  case None => fail("Data should actually contain data")
                }
              case None => fail("Expected to get one argument")
            }
          case _ => fail("Got wrong result")
        }
      }

      "return correct url entry based on identified page type" in {
        val url = coverifiedUrl
        val pageType = "url"
        val sourceId = "coverified"
        val selectors = validPageType.selectors

        /* Only checking correct type. Rest of content has been tested already */
        Analyzer invokePrivate buildEntry(
          url,
          validUrlPageDoc,
          pageType,
          sourceId,
          selectors
        ) match {
          case Success(field: SelectionBuilder.Field[_, _]) =>
            field.arguments.headOption match {
              case Some(argument) =>
                argument.value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.`type` shouldBe Some(EntryTypeType.url)
                  case _ => fail("Got wrong argument")
                }
              case None => fail("Wanted to get at least one argument")
            }
          case Success(_) => fail("Succeeded with wrong output")
          case Failure(exception) =>
            fail(
              "Building an entry was meant to succeed, but failed with exception.",
              exception
            )
        }
      }

      "return correct video entry based on identified page type" in {
        val url = coverifiedUrl
        val pageType = "video"
        val sourceId = "coverified"
        val selectors = validPageType.selectors

        /* Only checking correct type. Rest of content has been tested already */
        Analyzer invokePrivate buildEntry(
          url,
          validVideoPageDoc,
          pageType,
          sourceId,
          selectors
        ) match {
          case Success(field: SelectionBuilder.Field[_, _]) =>
            field.arguments.headOption match {
              case Some(argument) =>
                argument.value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.`type` shouldBe Some(EntryTypeType.video)
                  case _ => fail("Got wrong argument")
                }
              case None => fail("Wanted to get at least one argument")
            }
          case Success(_) => fail("Succeeded with wrong output")
          case Failure(exception) =>
            fail(
              "Building an entry was meant to succeed, but failed with exception.",
              exception
            )
        }
      }
    }

    "analysing received content" should {
      val analyze = PrivateMethod[Try[
        SelectionBuilder[RootMutation, Option[Extractor.EntryView]]
      ]](Symbol("analyze"))

      "skip pages, that are not meant to be analyzed" in {
        val profileConfig = ProfileConfig(
          Profile(
            coverifiedUrl,
            List(
              validPageType.copy(
                condition = validPageType.condition.copy(
                  path = Some("some invalid selector"),
                  selector = Some("some invalid path")
                )
              )
            )
          )
        )

        Analyzer invokePrivate analyze(
          coverifiedUrl + "/impressum/subpage",
          validUrlPageDoc,
          "coverified",
          profileConfig
        ) match {
          case Failure(exception: AnalysisException) =>
            exception.msg shouldBe s"Unable to gather profile config for url '${coverifiedUrl + "/impressum/subpage"}'."
          case Failure(exception) =>
            fail("Failed with wrong exception.", exception)
          case Success(_) =>
            fail("Analysis content of unsupported url was meant to fail.")
        }
      }

      "succeed, if analysis was successful" in {
        val url = coverifiedUrl
        val sourceId = "coverified"
        val profileConfig = ProfileConfig(
          Profile(
            coverifiedUrl,
            List(
              validPageType.copy(
                condition = validPageType.condition.copy(path = Some(url))
              )
            )
          )
        )

        Analyzer invokePrivate analyze(
          url,
          validUrlPageDoc,
          sourceId,
          profileConfig
        ) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Analysis was meant to succeed, but failed.", exception)
        }
      }
    }

    "running the analysis" should {
      val validUrl = coverifiedUrl
      val mockBrowser = new MockBrowser(Map(validUrl -> validUrlPageDoc))

      "return Failure, when browser fails" in {
        val profileConfig = getConfig(MockBrowser.dislikedUrl)
        Analyzer.run(
          MockBrowser.dislikedUrl,
          "coverified",
          profileConfig,
          mockBrowser
        ) match {
          case Failure(exception: DislikeThatUrlException) =>
            exception.msg shouldBe "I don't like that url."
          case Failure(exception) =>
            fail("Browser failed with wrong exception.", exception)
          case Success(_) => fail("Browser was meant to fail, but succeeded.")
        }
      }

      "return something, if browser does not fail and analysis does not fail" in {
        Analyzer.run(validUrl, "coverified", getConfig(validUrl), mockBrowser) match {
          case Success(_) => succeed
          case Failure(exception) =>
            fail("Browser did not fail, but analysis failed.", exception)
        }
      }
    }
  }
}
