/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.analyzer

import caliban.client.Operations.RootMutation
import caliban.client.{Argument, SelectionBuilder}
import info.coverified.extractor.config.ProfileConfigHelper
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
        breadcrumb = None,
        content = "#content",
        date = None,
        image = None,
        subtitle = Some("#subtitle"),
        summary = None,
        title = "#title",
        video = None
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
        PrivateMethod[Option[(String, Selectors)]](Symbol("determinePageType"))
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
          ) shouldBe None
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
            case Some((name, selectors)) =>
              name shouldBe validPageType.name
              selectors shouldBe validPageType.selectors
            case None => fail("Page type should be determined.")
          }
        }
      }
    }

    "building the entries with extracted information" should {
      val buildEntry =
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
        ]]]](Symbol("buildEntry"))

      "fail on attempt to extract information from unsupported page type" in {
        val url = coverifiedUrl
        val pageType = "unicornPageType"
        val pageDoc = mock[Document]
        val sourceId = "Some Source"
        val selectors = validPageType.selectors

        intercept[RuntimeException] {
          Analyzer invokePrivate buildEntry(
            url,
            pageDoc,
            pageType,
            sourceId,
            selectors
          )
        }.getMessage shouldBe s"Unknown page type: $pageType"
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
          case UrlViewInformation(title, subTitle, content, publishDate) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe Some("... with a subtitle")
            content shouldBe Some("And with all the content.")
            publishDate match {
              case Some(_) => succeed
              case None =>
                fail("Expected to get a dummy value as publication date")
            }
        }
      }

      "extract information correctly from url page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractUrlViewInformation(
            validUrlPageDocWithoutOptionalInformation,
            validPageType.selectors
          )
        ) {
          case UrlViewInformation(title, subTitle, content, publishDate) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe None
            content shouldBe None
            publishDate match {
              case Some(_) => succeed
              case None =>
                fail("Expected to get a dummy value as publication date")
            }
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
          case VideoViewInformation(title, subTitle, content, publishDate) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe Some("... with a subtitle")
            content shouldBe Some("And with all the content.")
            publishDate match {
              case Some(_) => succeed
              case None =>
                fail("Expected to get a dummy value as publication date")
            }
        }
      }

      "extract information correctly from video page, if optional entries are not apparent" in {
        inside(
          Analyzer invokePrivate extractVideoViewInformation(
            validVideoPageDocWithoutOptionalInformation,
            validPageType.selectors
          )
        ) {
          case VideoViewInformation(title, subTitle, content, publishDate) =>
            title shouldBe Some("Url page with all information available")
            subTitle shouldBe None
            content shouldBe None
            publishDate match {
              case Some(_) => succeed
              case None =>
                fail("Expected to get a dummy value as publication date")
            }
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
          Some("content"),
          Some("2020-05-23T11:00:00Z")
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
                    eci.image shouldBe None
                    eci.content shouldBe Some("content")
                    eci.summary shouldBe None
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
          Some("content"),
          Some("2020-05-23T11:00:00Z")
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
                    eci.image shouldBe None
                    eci.content shouldBe Some("content")
                    eci.summary shouldBe None
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
        inside(
          Analyzer invokePrivate buildEntry(
            url,
            validUrlPageDoc,
            pageType,
            sourceId,
            selectors
          )
        ) {
          case field: SelectionBuilder.Field[_, _] =>
            field.arguments.headOption match {
              case Some(argument) =>
                argument.value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.`type` shouldBe Some(EntryTypeType.url)
                  case _ => fail("Got wrong argument")
                }
              case None => fail("Wanted to get at least one argument")
            }
        }
      }

      "return correct video entry based on identified page type" in {
        val url = coverifiedUrl
        val pageType = "video"
        val sourceId = "coverified"
        val selectors = validPageType.selectors

        /* Only checking correct type. Rest of content has been tested already */
        inside(
          Analyzer invokePrivate buildEntry(
            url,
            validVideoPageDoc,
            pageType,
            sourceId,
            selectors
          )
        ) {
          case field: SelectionBuilder.Field[_, _] =>
            field.arguments.headOption match {
              case Some(argument) =>
                argument.value match {
                  case Some(eci: EntryCreateInput) =>
                    eci.`type` shouldBe Some(EntryTypeType.video)
                  case _ => fail("Got wrong argument")
                }
              case None => fail("Wanted to get at least one argument")
            }
        }
      }
    }

    "analysing received content" should {
      val analyze = PrivateMethod[Try[
        Option[SelectionBuilder[RootMutation, Option[Entry.EntryView[
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
        ]]]]
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
        ) shouldBe Success(None)
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
          case Success(Some(_)) => succeed
          case Success(_)       => fail("Analysis succeeded but without result.")
          case Failure(exception) =>
            fail("Analysis was meant to succeed, but failed.", exception)
        }
      }
    }

    "running the analysis" should {
      val validUrl = coverifiedUrl
      val mockBrowser = new MockBrowser(Map(validUrl -> validUrlPageDoc))

      "return None, when browser fails" in {
        val profileConfig = getConfig(MockBrowser.dislikedUrl)
        Analyzer.run(
          MockBrowser.dislikedUrl,
          "coverified",
          profileConfig,
          mockBrowser
        ) shouldBe None
      }

      "return something, if browser does not fail and analysis does not fail" in {
        Analyzer.run(validUrl, "coverified", getConfig(validUrl), mockBrowser) match {
          case Some(Some(_)) => succeed
          case Some(None)    => fail("Browser did not fail, but analysis failed")
          case None          => fail("Some result should have been returned")
        }
      }
    }
  }
}
