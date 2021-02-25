/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.schema

import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client._
import caliban.client.Operations._
import caliban.client.__Value._

object CoVerifiedClientSchema {

  type DateTime = String

  type ID = String

  type JSON = String

  type Upload = String

  sealed trait SortOrganizationsBy extends scala.Product with scala.Serializable

  object SortOrganizationsBy {

    case object id_ASC extends SortOrganizationsBy

    case object id_DESC extends SortOrganizationsBy

    case object name_ASC extends SortOrganizationsBy

    case object name_DESC extends SortOrganizationsBy

    case object updatedAt_ASC extends SortOrganizationsBy

    case object updatedAt_DESC extends SortOrganizationsBy

    case object createdAt_ASC extends SortOrganizationsBy

    case object createdAt_DESC extends SortOrganizationsBy

    implicit val decoder: ScalarDecoder[SortOrganizationsBy] = {
      case __StringValue("id_ASC")    => Right(SortOrganizationsBy.id_ASC)
      case __StringValue("id_DESC")   => Right(SortOrganizationsBy.id_DESC)
      case __StringValue("name_ASC")  => Right(SortOrganizationsBy.name_ASC)
      case __StringValue("name_DESC") => Right(SortOrganizationsBy.name_DESC)
      case __StringValue("updatedAt_ASC") =>
        Right(SortOrganizationsBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortOrganizationsBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") =>
        Right(SortOrganizationsBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortOrganizationsBy.createdAt_DESC)
      case other =>
        Left(
          DecodingError(s"Can't build SortOrganizationsBy from input $other")
        )
    }
    implicit val encoder: ArgEncoder[SortOrganizationsBy] =
      new ArgEncoder[SortOrganizationsBy] {
        override def encode(value: SortOrganizationsBy): __Value = value match {
          case SortOrganizationsBy.id_ASC        => __EnumValue("id_ASC")
          case SortOrganizationsBy.id_DESC       => __EnumValue("id_DESC")
          case SortOrganizationsBy.name_ASC      => __EnumValue("name_ASC")
          case SortOrganizationsBy.name_DESC     => __EnumValue("name_DESC")
          case SortOrganizationsBy.updatedAt_ASC => __EnumValue("updatedAt_ASC")
          case SortOrganizationsBy.updatedAt_DESC =>
            __EnumValue("updatedAt_DESC")
          case SortOrganizationsBy.createdAt_ASC => __EnumValue("createdAt_ASC")
          case SortOrganizationsBy.createdAt_DESC =>
            __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortOrganizationsBy"
      }
  }

  sealed trait SortTagsBy extends scala.Product with scala.Serializable

  object SortTagsBy {

    case object id_ASC extends SortTagsBy

    case object id_DESC extends SortTagsBy

    case object name_ASC extends SortTagsBy

    case object name_DESC extends SortTagsBy

    case object language_ASC extends SortTagsBy

    case object language_DESC extends SortTagsBy

    case object description_ASC extends SortTagsBy

    case object description_DESC extends SortTagsBy

    case object relevance_ASC extends SortTagsBy

    case object relevance_DESC extends SortTagsBy

    case object updatedAt_ASC extends SortTagsBy

    case object updatedAt_DESC extends SortTagsBy

    case object createdAt_ASC extends SortTagsBy

    case object createdAt_DESC extends SortTagsBy

    implicit val decoder: ScalarDecoder[SortTagsBy] = {
      case __StringValue("id_ASC")          => Right(SortTagsBy.id_ASC)
      case __StringValue("id_DESC")         => Right(SortTagsBy.id_DESC)
      case __StringValue("name_ASC")        => Right(SortTagsBy.name_ASC)
      case __StringValue("name_DESC")       => Right(SortTagsBy.name_DESC)
      case __StringValue("language_ASC")    => Right(SortTagsBy.language_ASC)
      case __StringValue("language_DESC")   => Right(SortTagsBy.language_DESC)
      case __StringValue("description_ASC") => Right(SortTagsBy.description_ASC)
      case __StringValue("description_DESC") =>
        Right(SortTagsBy.description_DESC)
      case __StringValue("relevance_ASC")  => Right(SortTagsBy.relevance_ASC)
      case __StringValue("relevance_DESC") => Right(SortTagsBy.relevance_DESC)
      case __StringValue("updatedAt_ASC")  => Right(SortTagsBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") => Right(SortTagsBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC")  => Right(SortTagsBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") => Right(SortTagsBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortTagsBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortTagsBy] = new ArgEncoder[SortTagsBy] {
      override def encode(value: SortTagsBy): __Value = value match {
        case SortTagsBy.id_ASC           => __EnumValue("id_ASC")
        case SortTagsBy.id_DESC          => __EnumValue("id_DESC")
        case SortTagsBy.name_ASC         => __EnumValue("name_ASC")
        case SortTagsBy.name_DESC        => __EnumValue("name_DESC")
        case SortTagsBy.language_ASC     => __EnumValue("language_ASC")
        case SortTagsBy.language_DESC    => __EnumValue("language_DESC")
        case SortTagsBy.description_ASC  => __EnumValue("description_ASC")
        case SortTagsBy.description_DESC => __EnumValue("description_DESC")
        case SortTagsBy.relevance_ASC    => __EnumValue("relevance_ASC")
        case SortTagsBy.relevance_DESC   => __EnumValue("relevance_DESC")
        case SortTagsBy.updatedAt_ASC    => __EnumValue("updatedAt_ASC")
        case SortTagsBy.updatedAt_DESC   => __EnumValue("updatedAt_DESC")
        case SortTagsBy.createdAt_ASC    => __EnumValue("createdAt_ASC")
        case SortTagsBy.createdAt_DESC   => __EnumValue("createdAt_DESC")
      }

      override def typeName: String = "SortTagsBy"
    }
  }

  sealed trait SortLanguagesBy extends scala.Product with scala.Serializable

  object SortLanguagesBy {

    case object id_ASC extends SortLanguagesBy

    case object id_DESC extends SortLanguagesBy

    case object name_ASC extends SortLanguagesBy

    case object name_DESC extends SortLanguagesBy

    case object updatedAt_ASC extends SortLanguagesBy

    case object updatedAt_DESC extends SortLanguagesBy

    case object createdAt_ASC extends SortLanguagesBy

    case object createdAt_DESC extends SortLanguagesBy

    implicit val decoder: ScalarDecoder[SortLanguagesBy] = {
      case __StringValue("id_ASC")    => Right(SortLanguagesBy.id_ASC)
      case __StringValue("id_DESC")   => Right(SortLanguagesBy.id_DESC)
      case __StringValue("name_ASC")  => Right(SortLanguagesBy.name_ASC)
      case __StringValue("name_DESC") => Right(SortLanguagesBy.name_DESC)
      case __StringValue("updatedAt_ASC") =>
        Right(SortLanguagesBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortLanguagesBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") =>
        Right(SortLanguagesBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortLanguagesBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortLanguagesBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortLanguagesBy] =
      new ArgEncoder[SortLanguagesBy] {
        override def encode(value: SortLanguagesBy): __Value = value match {
          case SortLanguagesBy.id_ASC         => __EnumValue("id_ASC")
          case SortLanguagesBy.id_DESC        => __EnumValue("id_DESC")
          case SortLanguagesBy.name_ASC       => __EnumValue("name_ASC")
          case SortLanguagesBy.name_DESC      => __EnumValue("name_DESC")
          case SortLanguagesBy.updatedAt_ASC  => __EnumValue("updatedAt_ASC")
          case SortLanguagesBy.updatedAt_DESC => __EnumValue("updatedAt_DESC")
          case SortLanguagesBy.createdAt_ASC  => __EnumValue("createdAt_ASC")
          case SortLanguagesBy.createdAt_DESC => __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortLanguagesBy"
      }
  }

  sealed trait SortGeoLocationsBy extends scala.Product with scala.Serializable

  object SortGeoLocationsBy {

    case object id_ASC extends SortGeoLocationsBy

    case object id_DESC extends SortGeoLocationsBy

    case object name_ASC extends SortGeoLocationsBy

    case object name_DESC extends SortGeoLocationsBy

    case object radius_ASC extends SortGeoLocationsBy

    case object radius_DESC extends SortGeoLocationsBy

    case object updatedAt_ASC extends SortGeoLocationsBy

    case object updatedAt_DESC extends SortGeoLocationsBy

    case object createdAt_ASC extends SortGeoLocationsBy

    case object createdAt_DESC extends SortGeoLocationsBy

    implicit val decoder: ScalarDecoder[SortGeoLocationsBy] = {
      case __StringValue("id_ASC")      => Right(SortGeoLocationsBy.id_ASC)
      case __StringValue("id_DESC")     => Right(SortGeoLocationsBy.id_DESC)
      case __StringValue("name_ASC")    => Right(SortGeoLocationsBy.name_ASC)
      case __StringValue("name_DESC")   => Right(SortGeoLocationsBy.name_DESC)
      case __StringValue("radius_ASC")  => Right(SortGeoLocationsBy.radius_ASC)
      case __StringValue("radius_DESC") => Right(SortGeoLocationsBy.radius_DESC)
      case __StringValue("updatedAt_ASC") =>
        Right(SortGeoLocationsBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortGeoLocationsBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") =>
        Right(SortGeoLocationsBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortGeoLocationsBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortGeoLocationsBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortGeoLocationsBy] =
      new ArgEncoder[SortGeoLocationsBy] {
        override def encode(value: SortGeoLocationsBy): __Value = value match {
          case SortGeoLocationsBy.id_ASC        => __EnumValue("id_ASC")
          case SortGeoLocationsBy.id_DESC       => __EnumValue("id_DESC")
          case SortGeoLocationsBy.name_ASC      => __EnumValue("name_ASC")
          case SortGeoLocationsBy.name_DESC     => __EnumValue("name_DESC")
          case SortGeoLocationsBy.radius_ASC    => __EnumValue("radius_ASC")
          case SortGeoLocationsBy.radius_DESC   => __EnumValue("radius_DESC")
          case SortGeoLocationsBy.updatedAt_ASC => __EnumValue("updatedAt_ASC")
          case SortGeoLocationsBy.updatedAt_DESC =>
            __EnumValue("updatedAt_DESC")
          case SortGeoLocationsBy.createdAt_ASC => __EnumValue("createdAt_ASC")
          case SortGeoLocationsBy.createdAt_DESC =>
            __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortGeoLocationsBy"
      }
  }

  sealed trait SortSourcesBy extends scala.Product with scala.Serializable

  object SortSourcesBy {

    case object id_ASC extends SortSourcesBy

    case object id_DESC extends SortSourcesBy

    case object name_ASC extends SortSourcesBy

    case object name_DESC extends SortSourcesBy

    case object url_ASC extends SortSourcesBy

    case object url_DESC extends SortSourcesBy

    case object location_ASC extends SortSourcesBy

    case object location_DESC extends SortSourcesBy

    case object description_ASC extends SortSourcesBy

    case object description_DESC extends SortSourcesBy

    case object updatedAt_ASC extends SortSourcesBy

    case object updatedAt_DESC extends SortSourcesBy

    case object createdAt_ASC extends SortSourcesBy

    case object createdAt_DESC extends SortSourcesBy

    implicit val decoder: ScalarDecoder[SortSourcesBy] = {
      case __StringValue("id_ASC")        => Right(SortSourcesBy.id_ASC)
      case __StringValue("id_DESC")       => Right(SortSourcesBy.id_DESC)
      case __StringValue("name_ASC")      => Right(SortSourcesBy.name_ASC)
      case __StringValue("name_DESC")     => Right(SortSourcesBy.name_DESC)
      case __StringValue("url_ASC")       => Right(SortSourcesBy.url_ASC)
      case __StringValue("url_DESC")      => Right(SortSourcesBy.url_DESC)
      case __StringValue("location_ASC")  => Right(SortSourcesBy.location_ASC)
      case __StringValue("location_DESC") => Right(SortSourcesBy.location_DESC)
      case __StringValue("description_ASC") =>
        Right(SortSourcesBy.description_ASC)
      case __StringValue("description_DESC") =>
        Right(SortSourcesBy.description_DESC)
      case __StringValue("updatedAt_ASC") => Right(SortSourcesBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortSourcesBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") => Right(SortSourcesBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortSourcesBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortSourcesBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortSourcesBy] =
      new ArgEncoder[SortSourcesBy] {
        override def encode(value: SortSourcesBy): __Value = value match {
          case SortSourcesBy.id_ASC           => __EnumValue("id_ASC")
          case SortSourcesBy.id_DESC          => __EnumValue("id_DESC")
          case SortSourcesBy.name_ASC         => __EnumValue("name_ASC")
          case SortSourcesBy.name_DESC        => __EnumValue("name_DESC")
          case SortSourcesBy.url_ASC          => __EnumValue("url_ASC")
          case SortSourcesBy.url_DESC         => __EnumValue("url_DESC")
          case SortSourcesBy.location_ASC     => __EnumValue("location_ASC")
          case SortSourcesBy.location_DESC    => __EnumValue("location_DESC")
          case SortSourcesBy.description_ASC  => __EnumValue("description_ASC")
          case SortSourcesBy.description_DESC => __EnumValue("description_DESC")
          case SortSourcesBy.updatedAt_ASC    => __EnumValue("updatedAt_ASC")
          case SortSourcesBy.updatedAt_DESC   => __EnumValue("updatedAt_DESC")
          case SortSourcesBy.createdAt_ASC    => __EnumValue("createdAt_ASC")
          case SortSourcesBy.createdAt_DESC   => __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortSourcesBy"
      }
  }

  sealed trait SortWidgetsBy extends scala.Product with scala.Serializable

  object SortWidgetsBy {

    case object id_ASC extends SortWidgetsBy

    case object id_DESC extends SortWidgetsBy

    case object name_ASC extends SortWidgetsBy

    case object name_DESC extends SortWidgetsBy

    case object organization_ASC extends SortWidgetsBy

    case object organization_DESC extends SortWidgetsBy

    case object language_ASC extends SortWidgetsBy

    case object language_DESC extends SortWidgetsBy

    case object sources_ASC extends SortWidgetsBy

    case object sources_DESC extends SortWidgetsBy

    case object updatedAt_ASC extends SortWidgetsBy

    case object updatedAt_DESC extends SortWidgetsBy

    case object createdAt_ASC extends SortWidgetsBy

    case object createdAt_DESC extends SortWidgetsBy

    implicit val decoder: ScalarDecoder[SortWidgetsBy] = {
      case __StringValue("id_ASC")    => Right(SortWidgetsBy.id_ASC)
      case __StringValue("id_DESC")   => Right(SortWidgetsBy.id_DESC)
      case __StringValue("name_ASC")  => Right(SortWidgetsBy.name_ASC)
      case __StringValue("name_DESC") => Right(SortWidgetsBy.name_DESC)
      case __StringValue("organization_ASC") =>
        Right(SortWidgetsBy.organization_ASC)
      case __StringValue("organization_DESC") =>
        Right(SortWidgetsBy.organization_DESC)
      case __StringValue("language_ASC")  => Right(SortWidgetsBy.language_ASC)
      case __StringValue("language_DESC") => Right(SortWidgetsBy.language_DESC)
      case __StringValue("sources_ASC")   => Right(SortWidgetsBy.sources_ASC)
      case __StringValue("sources_DESC")  => Right(SortWidgetsBy.sources_DESC)
      case __StringValue("updatedAt_ASC") => Right(SortWidgetsBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortWidgetsBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") => Right(SortWidgetsBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortWidgetsBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortWidgetsBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortWidgetsBy] =
      new ArgEncoder[SortWidgetsBy] {
        override def encode(value: SortWidgetsBy): __Value = value match {
          case SortWidgetsBy.id_ASC           => __EnumValue("id_ASC")
          case SortWidgetsBy.id_DESC          => __EnumValue("id_DESC")
          case SortWidgetsBy.name_ASC         => __EnumValue("name_ASC")
          case SortWidgetsBy.name_DESC        => __EnumValue("name_DESC")
          case SortWidgetsBy.organization_ASC => __EnumValue("organization_ASC")
          case SortWidgetsBy.organization_DESC =>
            __EnumValue("organization_DESC")
          case SortWidgetsBy.language_ASC   => __EnumValue("language_ASC")
          case SortWidgetsBy.language_DESC  => __EnumValue("language_DESC")
          case SortWidgetsBy.sources_ASC    => __EnumValue("sources_ASC")
          case SortWidgetsBy.sources_DESC   => __EnumValue("sources_DESC")
          case SortWidgetsBy.updatedAt_ASC  => __EnumValue("updatedAt_ASC")
          case SortWidgetsBy.updatedAt_DESC => __EnumValue("updatedAt_DESC")
          case SortWidgetsBy.createdAt_ASC  => __EnumValue("createdAt_ASC")
          case SortWidgetsBy.createdAt_DESC => __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortWidgetsBy"
      }
  }

  sealed trait SortEntriesBy extends scala.Product with scala.Serializable

  object SortEntriesBy {

    case object id_ASC extends SortEntriesBy

    case object id_DESC extends SortEntriesBy

    case object publishDate_ASC extends SortEntriesBy

    case object publishDate_DESC extends SortEntriesBy

    case object title_ASC extends SortEntriesBy

    case object title_DESC extends SortEntriesBy

    case object content_ASC extends SortEntriesBy

    case object content_DESC extends SortEntriesBy

    case object url_ASC extends SortEntriesBy

    case object url_DESC extends SortEntriesBy

    case object tags_ASC extends SortEntriesBy

    case object tags_DESC extends SortEntriesBy

    case object language_ASC extends SortEntriesBy

    case object language_DESC extends SortEntriesBy

    case object source_ASC extends SortEntriesBy

    case object source_DESC extends SortEntriesBy

    case object updatedAt_ASC extends SortEntriesBy

    case object updatedAt_DESC extends SortEntriesBy

    case object createdAt_ASC extends SortEntriesBy

    case object createdAt_DESC extends SortEntriesBy

    implicit val decoder: ScalarDecoder[SortEntriesBy] = {
      case __StringValue("id_ASC")  => Right(SortEntriesBy.id_ASC)
      case __StringValue("id_DESC") => Right(SortEntriesBy.id_DESC)
      case __StringValue("publishDate_ASC") =>
        Right(SortEntriesBy.publishDate_ASC)
      case __StringValue("publishDate_DESC") =>
        Right(SortEntriesBy.publishDate_DESC)
      case __StringValue("title_ASC")     => Right(SortEntriesBy.title_ASC)
      case __StringValue("title_DESC")    => Right(SortEntriesBy.title_DESC)
      case __StringValue("content_ASC")   => Right(SortEntriesBy.content_ASC)
      case __StringValue("content_DESC")  => Right(SortEntriesBy.content_DESC)
      case __StringValue("url_ASC")       => Right(SortEntriesBy.url_ASC)
      case __StringValue("url_DESC")      => Right(SortEntriesBy.url_DESC)
      case __StringValue("tags_ASC")      => Right(SortEntriesBy.tags_ASC)
      case __StringValue("tags_DESC")     => Right(SortEntriesBy.tags_DESC)
      case __StringValue("language_ASC")  => Right(SortEntriesBy.language_ASC)
      case __StringValue("language_DESC") => Right(SortEntriesBy.language_DESC)
      case __StringValue("source_ASC")    => Right(SortEntriesBy.source_ASC)
      case __StringValue("source_DESC")   => Right(SortEntriesBy.source_DESC)
      case __StringValue("updatedAt_ASC") => Right(SortEntriesBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") =>
        Right(SortEntriesBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC") => Right(SortEntriesBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") =>
        Right(SortEntriesBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortEntriesBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortEntriesBy] =
      new ArgEncoder[SortEntriesBy] {
        override def encode(value: SortEntriesBy): __Value = value match {
          case SortEntriesBy.id_ASC           => __EnumValue("id_ASC")
          case SortEntriesBy.id_DESC          => __EnumValue("id_DESC")
          case SortEntriesBy.publishDate_ASC  => __EnumValue("publishDate_ASC")
          case SortEntriesBy.publishDate_DESC => __EnumValue("publishDate_DESC")
          case SortEntriesBy.title_ASC        => __EnumValue("title_ASC")
          case SortEntriesBy.title_DESC       => __EnumValue("title_DESC")
          case SortEntriesBy.content_ASC      => __EnumValue("content_ASC")
          case SortEntriesBy.content_DESC     => __EnumValue("content_DESC")
          case SortEntriesBy.url_ASC          => __EnumValue("url_ASC")
          case SortEntriesBy.url_DESC         => __EnumValue("url_DESC")
          case SortEntriesBy.tags_ASC         => __EnumValue("tags_ASC")
          case SortEntriesBy.tags_DESC        => __EnumValue("tags_DESC")
          case SortEntriesBy.language_ASC     => __EnumValue("language_ASC")
          case SortEntriesBy.language_DESC    => __EnumValue("language_DESC")
          case SortEntriesBy.source_ASC       => __EnumValue("source_ASC")
          case SortEntriesBy.source_DESC      => __EnumValue("source_DESC")
          case SortEntriesBy.updatedAt_ASC    => __EnumValue("updatedAt_ASC")
          case SortEntriesBy.updatedAt_DESC   => __EnumValue("updatedAt_DESC")
          case SortEntriesBy.createdAt_ASC    => __EnumValue("createdAt_ASC")
          case SortEntriesBy.createdAt_DESC   => __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortEntriesBy"
      }
  }

  sealed trait SortUsersBy extends scala.Product with scala.Serializable

  object SortUsersBy {

    case object id_ASC extends SortUsersBy

    case object id_DESC extends SortUsersBy

    case object name_ASC extends SortUsersBy

    case object name_DESC extends SortUsersBy

    case object email_ASC extends SortUsersBy

    case object email_DESC extends SortUsersBy

    case object isAdmin_ASC extends SortUsersBy

    case object isAdmin_DESC extends SortUsersBy

    case object organization_ASC extends SortUsersBy

    case object organization_DESC extends SortUsersBy

    case object updatedAt_ASC extends SortUsersBy

    case object updatedAt_DESC extends SortUsersBy

    case object createdAt_ASC extends SortUsersBy

    case object createdAt_DESC extends SortUsersBy

    implicit val decoder: ScalarDecoder[SortUsersBy] = {
      case __StringValue("id_ASC")       => Right(SortUsersBy.id_ASC)
      case __StringValue("id_DESC")      => Right(SortUsersBy.id_DESC)
      case __StringValue("name_ASC")     => Right(SortUsersBy.name_ASC)
      case __StringValue("name_DESC")    => Right(SortUsersBy.name_DESC)
      case __StringValue("email_ASC")    => Right(SortUsersBy.email_ASC)
      case __StringValue("email_DESC")   => Right(SortUsersBy.email_DESC)
      case __StringValue("isAdmin_ASC")  => Right(SortUsersBy.isAdmin_ASC)
      case __StringValue("isAdmin_DESC") => Right(SortUsersBy.isAdmin_DESC)
      case __StringValue("organization_ASC") =>
        Right(SortUsersBy.organization_ASC)
      case __StringValue("organization_DESC") =>
        Right(SortUsersBy.organization_DESC)
      case __StringValue("updatedAt_ASC")  => Right(SortUsersBy.updatedAt_ASC)
      case __StringValue("updatedAt_DESC") => Right(SortUsersBy.updatedAt_DESC)
      case __StringValue("createdAt_ASC")  => Right(SortUsersBy.createdAt_ASC)
      case __StringValue("createdAt_DESC") => Right(SortUsersBy.createdAt_DESC)
      case other =>
        Left(DecodingError(s"Can't build SortUsersBy from input $other"))
    }
    implicit val encoder: ArgEncoder[SortUsersBy] =
      new ArgEncoder[SortUsersBy] {
        override def encode(value: SortUsersBy): __Value = value match {
          case SortUsersBy.id_ASC            => __EnumValue("id_ASC")
          case SortUsersBy.id_DESC           => __EnumValue("id_DESC")
          case SortUsersBy.name_ASC          => __EnumValue("name_ASC")
          case SortUsersBy.name_DESC         => __EnumValue("name_DESC")
          case SortUsersBy.email_ASC         => __EnumValue("email_ASC")
          case SortUsersBy.email_DESC        => __EnumValue("email_DESC")
          case SortUsersBy.isAdmin_ASC       => __EnumValue("isAdmin_ASC")
          case SortUsersBy.isAdmin_DESC      => __EnumValue("isAdmin_DESC")
          case SortUsersBy.organization_ASC  => __EnumValue("organization_ASC")
          case SortUsersBy.organization_DESC => __EnumValue("organization_DESC")
          case SortUsersBy.updatedAt_ASC     => __EnumValue("updatedAt_ASC")
          case SortUsersBy.updatedAt_DESC    => __EnumValue("updatedAt_DESC")
          case SortUsersBy.createdAt_ASC     => __EnumValue("createdAt_ASC")
          case SortUsersBy.createdAt_DESC    => __EnumValue("createdAt_DESC")
        }

        override def typeName: String = "SortUsersBy"
      }
  }

  sealed trait CacheControlScope extends scala.Product with scala.Serializable

  object CacheControlScope {

    case object PUBLIC extends CacheControlScope

    case object PRIVATE extends CacheControlScope

    implicit val decoder: ScalarDecoder[CacheControlScope] = {
      case __StringValue("PUBLIC")  => Right(CacheControlScope.PUBLIC)
      case __StringValue("PRIVATE") => Right(CacheControlScope.PRIVATE)
      case other =>
        Left(DecodingError(s"Can't build CacheControlScope from input $other"))
    }
    implicit val encoder: ArgEncoder[CacheControlScope] =
      new ArgEncoder[CacheControlScope] {
        override def encode(value: CacheControlScope): __Value = value match {
          case CacheControlScope.PUBLIC  => __EnumValue("PUBLIC")
          case CacheControlScope.PRIVATE => __EnumValue("PRIVATE")
        }

        override def typeName: String = "CacheControlScope"
      }
  }

  type Organization

  object Organization {

    final case class OrganizationView(
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection = SelectionBuilder[Organization, OrganizationView]

    def view: ViewSelection =
      (`_label_` ~ id ~ name ~ updatedAt ~ createdAt).map {
        case ((((_label__, id), name), updatedAt), createdAt) =>
          OrganizationView(_label__, id, name, updatedAt, createdAt)
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Organization List config, or
      * 2. As an alias to the field set on 'labelField' in the Organization List config, or
      * 3. As an alias to a 'name' field on the Organization List (if one exists), or
      * 4. As an alias to the 'id' field on the Organization List.
      */
    def `_label_` : SelectionBuilder[Organization, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Organization, String] = Field("id", Scalar())

    def name: SelectionBuilder[Organization, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def updatedAt: SelectionBuilder[Organization, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Organization, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type CloudinaryImage_File

  object CloudinaryImage_File {

    final case class CloudinaryImage_FileView(
        id: Option[String],
        path: Option[String],
        filename: Option[String],
        originalFilename: Option[String],
        mimetype: Option[String],
        encoding: Option[String],
        publicUrl: Option[String],
        publicUrlTransformed: Option[String]
    )

    type ViewSelection =
      SelectionBuilder[CloudinaryImage_File, CloudinaryImage_FileView]

    def view(
        publicUrlTransformedTransformation: Option[CloudinaryImageFormat] = None
    ): ViewSelection =
      (id ~ path ~ filename ~ originalFilename ~ mimetype ~ encoding ~ publicUrl ~ publicUrlTransformed(
        publicUrlTransformedTransformation
      )).map {
        case (
            (
              (
                ((((id, path), filename), originalFilename), mimetype),
                encoding
              ),
              publicUrl
            ),
            publicUrlTransformed
            ) =>
          CloudinaryImage_FileView(
            id,
            path,
            filename,
            originalFilename,
            mimetype,
            encoding,
            publicUrl,
            publicUrlTransformed
          )
      }

    def id: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("id", OptionOf(Scalar()))

    def path: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("path", OptionOf(Scalar()))

    def filename: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("filename", OptionOf(Scalar()))

    def originalFilename
        : SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("originalFilename", OptionOf(Scalar()))

    def mimetype: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("mimetype", OptionOf(Scalar()))

    def encoding: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("encoding", OptionOf(Scalar()))

    def publicUrl: SelectionBuilder[CloudinaryImage_File, Option[String]] =
      Field("publicUrl", OptionOf(Scalar()))

    def publicUrlTransformed(
        transformation: Option[CloudinaryImageFormat] = None
    ): SelectionBuilder[CloudinaryImage_File, Option[String]] = Field(
      "publicUrlTransformed",
      OptionOf(Scalar()),
      arguments = List(Argument("transformation", transformation))
    )
  }

  type Tag

  object Tag {

    final case class TagView[LanguageSelection, ImageSelection](
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        language: Option[LanguageSelection],
        description: Option[String],
        relevance: Option[Int],
        image: Option[ImageSelection],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[LanguageSelection, ImageSelection] =
      SelectionBuilder[Tag, TagView[LanguageSelection, ImageSelection]]

    def view[LanguageSelection, ImageSelection](
        languageSelection: SelectionBuilder[Language, LanguageSelection],
        imageSelection: SelectionBuilder[CloudinaryImage_File, ImageSelection]
    ): ViewSelection[LanguageSelection, ImageSelection] =
      (`_label_` ~ id ~ name ~ language(
        languageSelection
      ) ~ description ~ relevance ~ image(
        imageSelection
      ) ~ updatedAt ~ createdAt).map {
        case (
            (
              (
                (
                  ((((_label__, id), name), language), description),
                  relevance
                ),
                image
              ),
              updatedAt
            ),
            createdAt
            ) =>
          TagView(
            _label__,
            id,
            name,
            language,
            description,
            relevance,
            image,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Tag List config, or
      * 2. As an alias to the field set on 'labelField' in the Tag List config, or
      * 3. As an alias to a 'name' field on the Tag List (if one exists), or
      * 4. As an alias to the 'id' field on the Tag List.
      */
    def `_label_` : SelectionBuilder[Tag, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Tag, String] = Field("id", Scalar())

    def name: SelectionBuilder[Tag, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def language[A](
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[Tag, Option[A]] =
      Field("language", OptionOf(Obj(innerSelection)))

    def description: SelectionBuilder[Tag, Option[String]] =
      Field("description", OptionOf(Scalar()))

    def relevance: SelectionBuilder[Tag, Option[Int]] =
      Field("relevance", OptionOf(Scalar()))

    def image[A](
        innerSelection: SelectionBuilder[CloudinaryImage_File, A]
    ): SelectionBuilder[Tag, Option[A]] =
      Field("image", OptionOf(Obj(innerSelection)))

    def updatedAt: SelectionBuilder[Tag, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Tag, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type Language

  object Language {

    final case class LanguageView(
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection = SelectionBuilder[Language, LanguageView]

    def view: ViewSelection =
      (`_label_` ~ id ~ name ~ updatedAt ~ createdAt).map {
        case ((((_label__, id), name), updatedAt), createdAt) =>
          LanguageView(_label__, id, name, updatedAt, createdAt)
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Language List config, or
      * 2. As an alias to the field set on 'labelField' in the Language List config, or
      * 3. As an alias to a 'name' field on the Language List (if one exists), or
      * 4. As an alias to the 'id' field on the Language List.
      */
    def `_label_` : SelectionBuilder[Language, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Language, String] = Field("id", Scalar())

    def name: SelectionBuilder[Language, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def updatedAt: SelectionBuilder[Language, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Language, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type LocationGoogle

  object LocationGoogle {

    final case class LocationGoogleView(
        id: Option[String],
        googlePlaceID: Option[String],
        formattedAddress: Option[String],
        lat: Option[Double],
        lng: Option[Double]
    )

    type ViewSelection = SelectionBuilder[LocationGoogle, LocationGoogleView]

    def view: ViewSelection =
      (id ~ googlePlaceID ~ formattedAddress ~ lat ~ lng).map {
        case ((((id, googlePlaceID), formattedAddress), lat), lng) =>
          LocationGoogleView(id, googlePlaceID, formattedAddress, lat, lng)
      }

    def id: SelectionBuilder[LocationGoogle, Option[String]] =
      Field("id", OptionOf(Scalar()))

    def googlePlaceID: SelectionBuilder[LocationGoogle, Option[String]] =
      Field("googlePlaceID", OptionOf(Scalar()))

    def formattedAddress: SelectionBuilder[LocationGoogle, Option[String]] =
      Field("formattedAddress", OptionOf(Scalar()))

    def lat: SelectionBuilder[LocationGoogle, Option[Double]] =
      Field("lat", OptionOf(Scalar()))

    def lng: SelectionBuilder[LocationGoogle, Option[Double]] =
      Field("lng", OptionOf(Scalar()))
  }

  type GeoLocation

  object GeoLocation {

    final case class GeoLocationView[LocationSelection](
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        location: Option[LocationSelection],
        radius: Option[Double],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[LocationSelection] =
      SelectionBuilder[GeoLocation, GeoLocationView[LocationSelection]]

    def view[LocationSelection](
        locationSelection: SelectionBuilder[LocationGoogle, LocationSelection]
    ): ViewSelection[LocationSelection] =
      (`_label_` ~ id ~ name ~ location(
        locationSelection
      ) ~ radius ~ updatedAt ~ createdAt).map {
        case (
            (((((_label__, id), name), location), radius), updatedAt),
            createdAt
            ) =>
          GeoLocationView(
            _label__,
            id,
            name,
            location,
            radius,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the GeoLocation List config, or
      * 2. As an alias to the field set on 'labelField' in the GeoLocation List config, or
      * 3. As an alias to a 'name' field on the GeoLocation List (if one exists), or
      * 4. As an alias to the 'id' field on the GeoLocation List.
      */
    def `_label_` : SelectionBuilder[GeoLocation, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[GeoLocation, String] = Field("id", Scalar())

    def name: SelectionBuilder[GeoLocation, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def location[A](
        innerSelection: SelectionBuilder[LocationGoogle, A]
    ): SelectionBuilder[GeoLocation, Option[A]] =
      Field("location", OptionOf(Obj(innerSelection)))

    def radius: SelectionBuilder[GeoLocation, Option[Double]] =
      Field("radius", OptionOf(Scalar()))

    def updatedAt: SelectionBuilder[GeoLocation, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[GeoLocation, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type Source

  object Source {

    final case class SourceView[LocationSelection](
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        url: Option[String],
        location: Option[LocationSelection],
        description: Option[String],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[LocationSelection] =
      SelectionBuilder[Source, SourceView[LocationSelection]]

    def view[LocationSelection](
        locationSelection: SelectionBuilder[GeoLocation, LocationSelection]
    ): ViewSelection[LocationSelection] =
      (`_label_` ~ id ~ name ~ url ~ location(
        locationSelection
      ) ~ description ~ updatedAt ~ createdAt).map {
        case (
            (
              (((((_label__, id), name), url), location), description),
              updatedAt
            ),
            createdAt
            ) =>
          SourceView(
            _label__,
            id,
            name,
            url,
            location,
            description,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Source List config, or
      * 2. As an alias to the field set on 'labelField' in the Source List config, or
      * 3. As an alias to a 'name' field on the Source List (if one exists), or
      * 4. As an alias to the 'id' field on the Source List.
      */
    def `_label_` : SelectionBuilder[Source, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Source, String] = Field("id", Scalar())

    def name: SelectionBuilder[Source, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def url: SelectionBuilder[Source, Option[String]] =
      Field("url", OptionOf(Scalar()))

    def location[A](
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[Source, Option[A]] =
      Field("location", OptionOf(Obj(innerSelection)))

    def description: SelectionBuilder[Source, Option[String]] =
      Field("description", OptionOf(Scalar()))

    def updatedAt: SelectionBuilder[Source, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Source, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type Widget

  object Widget {

    final case class WidgetView[
        OrganizationSelection,
        LanguageSelection,
        SourcesSelection,
        _sourcesMetaSelection
    ](
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        organization: Option[OrganizationSelection],
        language: Option[LanguageSelection],
        sources: List[SourcesSelection],
        _sourcesMeta: Option[_sourcesMetaSelection],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[
        OrganizationSelection,
        LanguageSelection,
        SourcesSelection,
        _sourcesMetaSelection
    ] = SelectionBuilder[Widget, WidgetView[
      OrganizationSelection,
      LanguageSelection,
      SourcesSelection,
      _sourcesMetaSelection
    ]]

    def view[
        OrganizationSelection,
        LanguageSelection,
        SourcesSelection,
        _sourcesMetaSelection
    ](
        sourcesWhere: Option[SourceWhereInput] = None,
        sourcesSearch: Option[String] = None,
        sourcesSortBy: Option[List[SortSourcesBy]] = None,
        sourcesOrderBy: Option[String] = None,
        sourcesFirst: Option[Int] = None,
        sourcesSkip: Option[Int] = None,
        _sourcesMetaWhere: Option[SourceWhereInput] = None,
        _sourcesMetaSearch: Option[String] = None,
        _sourcesMetaSortBy: Option[List[SortSourcesBy]] = None,
        _sourcesMetaOrderBy: Option[String] = None,
        _sourcesMetaFirst: Option[Int] = None,
        _sourcesMetaSkip: Option[Int] = None
    )(
        organizationSelection: SelectionBuilder[
          Organization,
          OrganizationSelection
        ],
        languageSelection: SelectionBuilder[Language, LanguageSelection],
        sourcesSelection: SelectionBuilder[Source, SourcesSelection],
        _sourcesMetaSelection: SelectionBuilder[
          _QueryMeta,
          _sourcesMetaSelection
        ]
    ): ViewSelection[
      OrganizationSelection,
      LanguageSelection,
      SourcesSelection,
      _sourcesMetaSelection
    ] =
      (`_label_` ~ id ~ name ~ organization(organizationSelection) ~ language(
        languageSelection
      ) ~ sources(
        sourcesWhere,
        sourcesSearch,
        sourcesSortBy,
        sourcesOrderBy,
        sourcesFirst,
        sourcesSkip
      )(sourcesSelection) ~ _sourcesMeta(
        _sourcesMetaWhere,
        _sourcesMetaSearch,
        _sourcesMetaSortBy,
        _sourcesMetaOrderBy,
        _sourcesMetaFirst,
        _sourcesMetaSkip
      )(_sourcesMetaSelection) ~ updatedAt ~ createdAt).map {
        case (
            (
              (
                (((((_label__, id), name), organization), language), sources),
                _sourcesMeta
              ),
              updatedAt
            ),
            createdAt
            ) =>
          WidgetView(
            _label__,
            id,
            name,
            organization,
            language,
            sources,
            _sourcesMeta,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Widget List config, or
      * 2. As an alias to the field set on 'labelField' in the Widget List config, or
      * 3. As an alias to a 'name' field on the Widget List (if one exists), or
      * 4. As an alias to the 'id' field on the Widget List.
      */
    def `_label_` : SelectionBuilder[Widget, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Widget, String] = Field("id", Scalar())

    def name: SelectionBuilder[Widget, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def organization[A](
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[Widget, Option[A]] =
      Field("organization", OptionOf(Obj(innerSelection)))

    def language[A](
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[Widget, Option[A]] =
      Field("language", OptionOf(Obj(innerSelection)))

    def sources[A](
        where: Option[SourceWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortSourcesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[Widget, List[A]] = Field(
      "sources",
      ListOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    def _sourcesMeta[A](
        where: Option[SourceWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortSourcesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[Widget, Option[A]] = Field(
      "_sourcesMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    def updatedAt: SelectionBuilder[Widget, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Widget, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type Entry

  object Entry {

    final case class EntryView[
        ImageSelection,
        TagsSelection,
        _tagsMetaSelection,
        LanguageSelection,
        SourceSelection
    ](
        `_label_`: Option[String],
        id: String,
        publishDate: Option[String],
        title: Option[String],
        image: Option[ImageSelection],
        content: Option[String],
        url: Option[String],
        tags: List[TagsSelection],
        _tagsMeta: Option[_tagsMetaSelection],
        language: Option[LanguageSelection],
        source: Option[SourceSelection],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[
        ImageSelection,
        TagsSelection,
        _tagsMetaSelection,
        LanguageSelection,
        SourceSelection
    ] = SelectionBuilder[Entry, EntryView[
      ImageSelection,
      TagsSelection,
      _tagsMetaSelection,
      LanguageSelection,
      SourceSelection
    ]]

    def view[
        ImageSelection,
        TagsSelection,
        _tagsMetaSelection,
        LanguageSelection,
        SourceSelection
    ](
        tagsWhere: Option[TagWhereInput] = None,
        tagsSearch: Option[String] = None,
        tagsSortBy: Option[List[SortTagsBy]] = None,
        tagsOrderBy: Option[String] = None,
        tagsFirst: Option[Int] = None,
        tagsSkip: Option[Int] = None,
        _tagsMetaWhere: Option[TagWhereInput] = None,
        _tagsMetaSearch: Option[String] = None,
        _tagsMetaSortBy: Option[List[SortTagsBy]] = None,
        _tagsMetaOrderBy: Option[String] = None,
        _tagsMetaFirst: Option[Int] = None,
        _tagsMetaSkip: Option[Int] = None
    )(
        imageSelection: SelectionBuilder[CloudinaryImage_File, ImageSelection],
        tagsSelection: SelectionBuilder[Tag, TagsSelection],
        _tagsMetaSelection: SelectionBuilder[_QueryMeta, _tagsMetaSelection],
        languageSelection: SelectionBuilder[Language, LanguageSelection],
        sourceSelection: SelectionBuilder[Source, SourceSelection]
    ): ViewSelection[
      ImageSelection,
      TagsSelection,
      _tagsMetaSelection,
      LanguageSelection,
      SourceSelection
    ] =
      (`_label_` ~ id ~ publishDate ~ title ~ image(
        imageSelection
      ) ~ content ~ url ~ tags(
        tagsWhere,
        tagsSearch,
        tagsSortBy,
        tagsOrderBy,
        tagsFirst,
        tagsSkip
      )(tagsSelection) ~ _tagsMeta(
        _tagsMetaWhere,
        _tagsMetaSearch,
        _tagsMetaSortBy,
        _tagsMetaOrderBy,
        _tagsMetaFirst,
        _tagsMetaSkip
      )(_tagsMetaSelection) ~ language(languageSelection) ~ source(
        sourceSelection
      ) ~ updatedAt ~ createdAt).map {
        case (
            (
              (
                (
                  (
                    (
                      (
                        (
                          ((((_label__, id), publishDate), title), image),
                          content
                        ),
                        url
                      ),
                      tags
                    ),
                    _tagsMeta
                  ),
                  language
                ),
                source
              ),
              updatedAt
            ),
            createdAt
            ) =>
          EntryView(
            _label__,
            id,
            publishDate,
            title,
            image,
            content,
            url,
            tags,
            _tagsMeta,
            language,
            source,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the Entry List config, or
      * 2. As an alias to the field set on 'labelField' in the Entry List config, or
      * 3. As an alias to a 'name' field on the Entry List (if one exists), or
      * 4. As an alias to the 'id' field on the Entry List.
      */
    def `_label_` : SelectionBuilder[Entry, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[Entry, String] = Field("id", Scalar())

    def publishDate: SelectionBuilder[Entry, Option[String]] =
      Field("publishDate", OptionOf(Scalar()))

    def title: SelectionBuilder[Entry, Option[String]] =
      Field("title", OptionOf(Scalar()))

    def image[A](
        innerSelection: SelectionBuilder[CloudinaryImage_File, A]
    ): SelectionBuilder[Entry, Option[A]] =
      Field("image", OptionOf(Obj(innerSelection)))

    def content: SelectionBuilder[Entry, Option[String]] =
      Field("content", OptionOf(Scalar()))

    def url: SelectionBuilder[Entry, Option[String]] =
      Field("url", OptionOf(Scalar()))

    def tags[A](
        where: Option[TagWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortTagsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[Entry, List[A]] = Field(
      "tags",
      ListOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    def _tagsMeta[A](
        where: Option[TagWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortTagsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[Entry, Option[A]] = Field(
      "_tagsMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    def language[A](
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[Entry, Option[A]] =
      Field("language", OptionOf(Obj(innerSelection)))

    def source[A](
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[Entry, Option[A]] =
      Field("source", OptionOf(Obj(innerSelection)))

    def updatedAt: SelectionBuilder[Entry, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[Entry, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type User

  object User {

    final case class UserView[OrganizationSelection](
        `_label_`: Option[String],
        id: String,
        name: Option[String],
        email: Option[String],
        isAdmin: Option[Boolean],
        password_is_set: Option[Boolean],
        organization: Option[OrganizationSelection],
        updatedAt: Option[DateTime],
        createdAt: Option[DateTime]
    )

    type ViewSelection[OrganizationSelection] =
      SelectionBuilder[User, UserView[OrganizationSelection]]

    def view[OrganizationSelection](
        organizationSelection: SelectionBuilder[
          Organization,
          OrganizationSelection
        ]
    ): ViewSelection[OrganizationSelection] =
      (`_label_` ~ id ~ name ~ email ~ isAdmin ~ password_is_set ~ organization(
        organizationSelection
      ) ~ updatedAt ~ createdAt).map {
        case (
            (
              (
                (((((_label__, id), name), email), isAdmin), password_is_set),
                organization
              ),
              updatedAt
            ),
            createdAt
            ) =>
          UserView(
            _label__,
            id,
            name,
            email,
            isAdmin,
            password_is_set,
            organization,
            updatedAt,
            createdAt
          )
      }

    /** This virtual field will be resolved in one of the following ways (in this order):
      * 1. Execution of 'labelResolver' set on the User List config, or
      * 2. As an alias to the field set on 'labelField' in the User List config, or
      * 3. As an alias to a 'name' field on the User List (if one exists), or
      * 4. As an alias to the 'id' field on the User List.
      */
    def `_label_` : SelectionBuilder[User, Option[String]] =
      Field("_label_", OptionOf(Scalar()))

    def id: SelectionBuilder[User, String] = Field("id", Scalar())

    def name: SelectionBuilder[User, Option[String]] =
      Field("name", OptionOf(Scalar()))

    def email: SelectionBuilder[User, Option[String]] =
      Field("email", OptionOf(Scalar()))

    def isAdmin: SelectionBuilder[User, Option[Boolean]] =
      Field("isAdmin", OptionOf(Scalar()))

    def password_is_set: SelectionBuilder[User, Option[Boolean]] =
      Field("password_is_set", OptionOf(Scalar()))

    def organization[A](
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[User, Option[A]] =
      Field("organization", OptionOf(Obj(innerSelection)))

    def updatedAt: SelectionBuilder[User, Option[DateTime]] =
      Field("updatedAt", OptionOf(Scalar()))

    def createdAt: SelectionBuilder[User, Option[DateTime]] =
      Field("createdAt", OptionOf(Scalar()))
  }

  type _ListAccess

  object _ListAccess {

    final case class _ListAccessView(
        create: Option[Boolean],
        read: Option[JSON],
        update: Option[JSON],
        delete: Option[JSON],
        auth: Option[JSON]
    )

    type ViewSelection = SelectionBuilder[_ListAccess, _ListAccessView]

    def view: ViewSelection = (create ~ read ~ update ~ delete ~ auth).map {
      case ((((create, read), update), delete), auth) =>
        _ListAccessView(create, read, update, delete, auth)
    }

    /** Access Control settings for the currently logged in (or anonymous)
      * user when performing 'create' operations.
      * NOTE: 'create' can only return a Boolean.
      * It is not possible to specify a declarative Where clause for this
      * operation
      */
    def create: SelectionBuilder[_ListAccess, Option[Boolean]] =
      Field("create", OptionOf(Scalar()))

    /** Access Control settings for the currently logged in (or anonymous)
      * user when performing 'read' operations.
      */
    def read: SelectionBuilder[_ListAccess, Option[JSON]] =
      Field("read", OptionOf(Scalar()))

    /** Access Control settings for the currently logged in (or anonymous)
      * user when performing 'update' operations.
      */
    def update: SelectionBuilder[_ListAccess, Option[JSON]] =
      Field("update", OptionOf(Scalar()))

    /** Access Control settings for the currently logged in (or anonymous)
      * user when performing 'delete' operations.
      */
    def delete: SelectionBuilder[_ListAccess, Option[JSON]] =
      Field("delete", OptionOf(Scalar()))

    /** Access Control settings for the currently logged in (or anonymous)
      * user when performing 'auth' operations.
      */
    def auth: SelectionBuilder[_ListAccess, Option[JSON]] =
      Field("auth", OptionOf(Scalar()))
  }

  type _ListQueries

  object _ListQueries {

    final case class _ListQueriesView(
        item: Option[String],
        list: Option[String],
        meta: Option[String]
    )

    type ViewSelection = SelectionBuilder[_ListQueries, _ListQueriesView]

    def view: ViewSelection = (item ~ list ~ meta).map {
      case ((item, list), meta) => _ListQueriesView(item, list, meta)
    }

    /** Single-item query name
      */
    def item: SelectionBuilder[_ListQueries, Option[String]] =
      Field("item", OptionOf(Scalar()))

    /** All-items query name
      */
    def list: SelectionBuilder[_ListQueries, Option[String]] =
      Field("list", OptionOf(Scalar()))

    /** List metadata query name
      */
    def meta: SelectionBuilder[_ListQueries, Option[String]] =
      Field("meta", OptionOf(Scalar()))
  }

  type _ListMutations

  object _ListMutations {

    final case class _ListMutationsView(
        create: Option[String],
        createMany: Option[String],
        update: Option[String],
        updateMany: Option[String],
        delete: Option[String],
        deleteMany: Option[String]
    )

    type ViewSelection = SelectionBuilder[_ListMutations, _ListMutationsView]

    def view: ViewSelection =
      (create ~ createMany ~ update ~ updateMany ~ delete ~ deleteMany).map {
        case (
            ((((create, createMany), update), updateMany), delete),
            deleteMany
            ) =>
          _ListMutationsView(
            create,
            createMany,
            update,
            updateMany,
            delete,
            deleteMany
          )
      }

    /** Create mutation name
      */
    def create: SelectionBuilder[_ListMutations, Option[String]] =
      Field("create", OptionOf(Scalar()))

    /** Create many mutation name
      */
    def createMany: SelectionBuilder[_ListMutations, Option[String]] =
      Field("createMany", OptionOf(Scalar()))

    /** Update mutation name
      */
    def update: SelectionBuilder[_ListMutations, Option[String]] =
      Field("update", OptionOf(Scalar()))

    /** Update many mutation name
      */
    def updateMany: SelectionBuilder[_ListMutations, Option[String]] =
      Field("updateMany", OptionOf(Scalar()))

    /** Delete mutation name
      */
    def delete: SelectionBuilder[_ListMutations, Option[String]] =
      Field("delete", OptionOf(Scalar()))

    /** Delete many mutation name
      */
    def deleteMany: SelectionBuilder[_ListMutations, Option[String]] =
      Field("deleteMany", OptionOf(Scalar()))
  }

  type _ListInputTypes

  object _ListInputTypes {

    final case class _ListInputTypesView(
        whereInput: Option[String],
        whereUniqueInput: Option[String],
        createInput: Option[String],
        createManyInput: Option[String],
        updateInput: Option[String],
        updateManyInput: Option[String]
    )

    type ViewSelection = SelectionBuilder[_ListInputTypes, _ListInputTypesView]

    def view: ViewSelection =
      (whereInput ~ whereUniqueInput ~ createInput ~ createManyInput ~ updateInput ~ updateManyInput)
        .map {
          case (
              (
                (
                  ((whereInput, whereUniqueInput), createInput),
                  createManyInput
                ),
                updateInput
              ),
              updateManyInput
              ) =>
            _ListInputTypesView(
              whereInput,
              whereUniqueInput,
              createInput,
              createManyInput,
              updateInput,
              updateManyInput
            )
        }

    /** Input type for matching multiple items
      */
    def whereInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("whereInput", OptionOf(Scalar()))

    /** Input type for matching a unique item
      */
    def whereUniqueInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("whereUniqueInput", OptionOf(Scalar()))

    /** Create mutation input type name
      */
    def createInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("createInput", OptionOf(Scalar()))

    /** Create many mutation input type name
      */
    def createManyInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("createManyInput", OptionOf(Scalar()))

    /** Update mutation name input
      */
    def updateInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("updateInput", OptionOf(Scalar()))

    /** Update many mutation name input
      */
    def updateManyInput: SelectionBuilder[_ListInputTypes, Option[String]] =
      Field("updateManyInput", OptionOf(Scalar()))
  }

  type _ListSchemaFields

  object _ListSchemaFields {

    final case class _ListSchemaFieldsView(
        path: Option[String],
        name: Option[String],
        `type`: Option[String]
    )

    type ViewSelection =
      SelectionBuilder[_ListSchemaFields, _ListSchemaFieldsView]

    def view: ViewSelection = (path ~ name ~ `type`).map {
      case ((path, name), type_) => _ListSchemaFieldsView(path, name, type_)
    }

    /** The path of the field in its list
      */
    def path: SelectionBuilder[_ListSchemaFields, Option[String]] =
      Field("path", OptionOf(Scalar()))

    /** The name of the field in its list
      */
    @deprecated("Use `path` instead", "")
    def name: SelectionBuilder[_ListSchemaFields, Option[String]] =
      Field("name", OptionOf(Scalar()))

    /** The field type (ie, Checkbox, Text, etc)
      */
    def `type`: SelectionBuilder[_ListSchemaFields, Option[String]] =
      Field("type", OptionOf(Scalar()))
  }

  type _ListSchemaRelatedFields

  object _ListSchemaRelatedFields {

    final case class _ListSchemaRelatedFieldsView(
        `type`: Option[String],
        fields: Option[List[Option[String]]]
    )

    type ViewSelection =
      SelectionBuilder[_ListSchemaRelatedFields, _ListSchemaRelatedFieldsView]

    def view: ViewSelection = (`type` ~ fields).map {
      case (type_, fields) =>
        _ListSchemaRelatedFieldsView(type_, fields)
    }

    /** The typename as used in GraphQL queries
      */
    def `type`: SelectionBuilder[_ListSchemaRelatedFields, Option[String]] =
      Field("type", OptionOf(Scalar()))

    /** A list of GraphQL field names
      */
    def fields: SelectionBuilder[_ListSchemaRelatedFields, Option[
      List[Option[String]]
    ]] = Field("fields", OptionOf(ListOf(OptionOf(Scalar()))))
  }

  type _ListSchema

  object _ListSchema {

    final case class _ListSchemaView[
        QueriesSelection,
        MutationsSelection,
        InputTypesSelection,
        FieldsSelection,
        RelatedFieldsSelection
    ](
        `type`: Option[String],
        queries: Option[QueriesSelection],
        mutations: Option[MutationsSelection],
        inputTypes: Option[InputTypesSelection],
        fields: Option[List[Option[FieldsSelection]]],
        relatedFields: Option[List[Option[RelatedFieldsSelection]]]
    )

    type ViewSelection[
        QueriesSelection,
        MutationsSelection,
        InputTypesSelection,
        FieldsSelection,
        RelatedFieldsSelection
    ] = SelectionBuilder[_ListSchema, _ListSchemaView[
      QueriesSelection,
      MutationsSelection,
      InputTypesSelection,
      FieldsSelection,
      RelatedFieldsSelection
    ]]

    def view[
        QueriesSelection,
        MutationsSelection,
        InputTypesSelection,
        FieldsSelection,
        RelatedFieldsSelection
    ](fieldsWhere: Option[_ListSchemaFieldsInput] = None)(
        queriesSelection: SelectionBuilder[_ListQueries, QueriesSelection],
        mutationsSelection: SelectionBuilder[
          _ListMutations,
          MutationsSelection
        ],
        inputTypesSelection: SelectionBuilder[
          _ListInputTypes,
          InputTypesSelection
        ],
        fieldsSelection: SelectionBuilder[_ListSchemaFields, FieldsSelection],
        relatedFieldsSelection: SelectionBuilder[
          _ListSchemaRelatedFields,
          RelatedFieldsSelection
        ]
    ): ViewSelection[
      QueriesSelection,
      MutationsSelection,
      InputTypesSelection,
      FieldsSelection,
      RelatedFieldsSelection
    ] =
      (`type` ~ queries(queriesSelection) ~ mutations(
        mutationsSelection
      ) ~ inputTypes(inputTypesSelection) ~ fields(fieldsWhere)(
        fieldsSelection
      ) ~ relatedFields(relatedFieldsSelection)).map {
        case (
            ((((type_, queries), mutations), inputTypes), fields),
            relatedFields
            ) =>
          _ListSchemaView(
            type_,
            queries,
            mutations,
            inputTypes,
            fields,
            relatedFields
          )
      }

    /** The typename as used in GraphQL queries
      */
    def `type`: SelectionBuilder[_ListSchema, Option[String]] =
      Field("type", OptionOf(Scalar()))

    /** Top level GraphQL query names which either return this type, or
      * provide aggregate information about this type
      */
    def queries[A](
        innerSelection: SelectionBuilder[_ListQueries, A]
    ): SelectionBuilder[_ListSchema, Option[A]] =
      Field("queries", OptionOf(Obj(innerSelection)))

    /** Top-level GraphQL mutation names
      */
    def mutations[A](
        innerSelection: SelectionBuilder[_ListMutations, A]
    ): SelectionBuilder[_ListSchema, Option[A]] =
      Field("mutations", OptionOf(Obj(innerSelection)))

    /** Top-level GraphQL input types
      */
    def inputTypes[A](
        innerSelection: SelectionBuilder[_ListInputTypes, A]
    ): SelectionBuilder[_ListSchema, Option[A]] =
      Field("inputTypes", OptionOf(Obj(innerSelection)))

    /** Information about fields defined on this list
      */
    def fields[A](where: Option[_ListSchemaFieldsInput] = None)(
        innerSelection: SelectionBuilder[_ListSchemaFields, A]
    ): SelectionBuilder[_ListSchema, Option[List[Option[A]]]] = Field(
      "fields",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("where", where))
    )

    /** Information about fields on other types which return this type, or
      * provide aggregate information about this type
      */
    def relatedFields[A](
        innerSelection: SelectionBuilder[_ListSchemaRelatedFields, A]
    ): SelectionBuilder[_ListSchema, Option[List[Option[A]]]] =
      Field("relatedFields", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
  }

  type _ListMeta

  object _ListMeta {

    final case class _ListMetaView[AccessSelection, SchemaSelection](
        key: Option[String],
        name: Option[String],
        description: Option[String],
        label: Option[String],
        singular: Option[String],
        plural: Option[String],
        path: Option[String],
        access: Option[AccessSelection],
        schema: Option[SchemaSelection]
    )

    type ViewSelection[AccessSelection, SchemaSelection] = SelectionBuilder[
      _ListMeta,
      _ListMetaView[AccessSelection, SchemaSelection]
    ]

    def view[AccessSelection, SchemaSelection](
        accessSelection: SelectionBuilder[_ListAccess, AccessSelection],
        schemaSelection: SelectionBuilder[_ListSchema, SchemaSelection]
    ): ViewSelection[AccessSelection, SchemaSelection] =
      (key ~ name ~ description ~ label ~ singular ~ plural ~ path ~ access(
        accessSelection
      ) ~ schema(schemaSelection)).map {
        case (
            (
              (
                (((((key, name), description), label), singular), plural),
                path
              ),
              access
            ),
            schema
            ) =>
          _ListMetaView(
            key,
            name,
            description,
            label,
            singular,
            plural,
            path,
            access,
            schema
          )
      }

    /** The Keystone list key
      */
    def key: SelectionBuilder[_ListMeta, Option[String]] =
      Field("key", OptionOf(Scalar()))

    /** The Keystone List name
      */
    @deprecated("Use `key` instead", "")
    def name: SelectionBuilder[_ListMeta, Option[String]] =
      Field("name", OptionOf(Scalar()))

    /** The list's user-facing description
      */
    def description: SelectionBuilder[_ListMeta, Option[String]] =
      Field("description", OptionOf(Scalar()))

    /** The list's display name in the Admin UI
      */
    def label: SelectionBuilder[_ListMeta, Option[String]] =
      Field("label", OptionOf(Scalar()))

    /** The list's singular display name
      */
    def singular: SelectionBuilder[_ListMeta, Option[String]] =
      Field("singular", OptionOf(Scalar()))

    /** The list's plural display name
      */
    def plural: SelectionBuilder[_ListMeta, Option[String]] =
      Field("plural", OptionOf(Scalar()))

    /** The list's data path
      */
    def path: SelectionBuilder[_ListMeta, Option[String]] =
      Field("path", OptionOf(Scalar()))

    /** Access control configuration for the currently authenticated request
      */
    def access[A](
        innerSelection: SelectionBuilder[_ListAccess, A]
    ): SelectionBuilder[_ListMeta, Option[A]] =
      Field("access", OptionOf(Obj(innerSelection)))

    /** Information on the generated GraphQL schema
      */
    def schema[A](
        innerSelection: SelectionBuilder[_ListSchema, A]
    ): SelectionBuilder[_ListMeta, Option[A]] =
      Field("schema", OptionOf(Obj(innerSelection)))
  }

  type _QueryMeta

  object _QueryMeta {

    final case class _QueryMetaView(count: Option[Int])

    type ViewSelection = SelectionBuilder[_QueryMeta, _QueryMetaView]

    def view: ViewSelection = count.map(count => _QueryMetaView(count))

    def count: SelectionBuilder[_QueryMeta, Option[Int]] =
      Field("count", OptionOf(Scalar()))
  }

  type unauthenticateUserOutput

  object unauthenticateUserOutput {

    final case class unauthenticateUserOutputView(success: Option[Boolean])

    type ViewSelection =
      SelectionBuilder[unauthenticateUserOutput, unauthenticateUserOutputView]

    def view: ViewSelection =
      success.map(success => unauthenticateUserOutputView(success))

    /** `true` when unauthentication succeeds.
      * NOTE: unauthentication always succeeds when the request has an invalid or missing authentication token.
      */
    def success: SelectionBuilder[unauthenticateUserOutput, Option[Boolean]] =
      Field("success", OptionOf(Scalar()))
  }

  type authenticateUserOutput

  object authenticateUserOutput {

    final case class authenticateUserOutputView[ItemSelection](
        token: Option[String],
        item: Option[ItemSelection]
    )

    type ViewSelection[ItemSelection] =
      SelectionBuilder[authenticateUserOutput, authenticateUserOutputView[
        ItemSelection
      ]]

    def view[ItemSelection](
        itemSelection: SelectionBuilder[User, ItemSelection]
    ): ViewSelection[ItemSelection] = (token ~ item(itemSelection)).map {
      case (token, item) => authenticateUserOutputView(token, item)
    }

    /** Used to make subsequent authenticated requests by setting this token in a header: 'Authorization: Bearer <token>'.
      */
    def token: SelectionBuilder[authenticateUserOutput, Option[String]] =
      Field("token", OptionOf(Scalar()))

    /** Retrieve information on the newly authenticated User here.
      */
    def item[A](
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[authenticateUserOutput, Option[A]] =
      Field("item", OptionOf(Obj(innerSelection)))
  }

  case class OrganizationWhereInput(
      AND: Option[List[Option[OrganizationWhereInput]]] = None,
      OR: Option[List[Option[OrganizationWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object OrganizationWhereInput {
    implicit val encoder: ArgEncoder[OrganizationWhereInput] =
      new ArgEncoder[OrganizationWhereInput] {
        override def encode(value: OrganizationWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "OrganizationWhereInput"
      }
  }

  case class OrganizationWhereUniqueInput(id: String)

  object OrganizationWhereUniqueInput {
    implicit val encoder: ArgEncoder[OrganizationWhereUniqueInput] =
      new ArgEncoder[OrganizationWhereUniqueInput] {
        override def encode(value: OrganizationWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "OrganizationWhereUniqueInput"
      }
  }

  case class OrganizationUpdateInput(name: Option[String] = None)

  object OrganizationUpdateInput {
    implicit val encoder: ArgEncoder[OrganizationUpdateInput] =
      new ArgEncoder[OrganizationUpdateInput] {
        override def encode(value: OrganizationUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "OrganizationUpdateInput"
      }
  }

  case class OrganizationsUpdateInput(
      id: String,
      data: Option[OrganizationUpdateInput] = None
  )

  object OrganizationsUpdateInput {
    implicit val encoder: ArgEncoder[OrganizationsUpdateInput] =
      new ArgEncoder[OrganizationsUpdateInput] {
        override def encode(value: OrganizationsUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "OrganizationsUpdateInput"
      }
  }

  case class OrganizationCreateInput(name: Option[String] = None)

  object OrganizationCreateInput {
    implicit val encoder: ArgEncoder[OrganizationCreateInput] =
      new ArgEncoder[OrganizationCreateInput] {
        override def encode(value: OrganizationCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "OrganizationCreateInput"
      }
  }

  case class OrganizationsCreateInput(
      data: Option[OrganizationCreateInput] = None
  )

  object OrganizationsCreateInput {
    implicit val encoder: ArgEncoder[OrganizationsCreateInput] =
      new ArgEncoder[OrganizationsCreateInput] {
        override def encode(value: OrganizationsCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "OrganizationsCreateInput"
      }
  }

  case class LanguageRelateToOneInput(
      create: Option[LanguageCreateInput] = None,
      connect: Option[LanguageWhereUniqueInput] = None,
      disconnect: Option[LanguageWhereUniqueInput] = None,
      disconnectAll: Option[Boolean] = None
  )

  object LanguageRelateToOneInput {
    implicit val encoder: ArgEncoder[LanguageRelateToOneInput] =
      new ArgEncoder[LanguageRelateToOneInput] {
        override def encode(value: LanguageRelateToOneInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageCreateInput]].encode(value)
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageWhereUniqueInput]].encode(value)
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageWhereUniqueInput]].encode(value)
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "LanguageRelateToOneInput"
      }
  }

  case class CloudinaryImageFormat(
      prettyName: Option[String] = None,
      width: Option[String] = None,
      height: Option[String] = None,
      crop: Option[String] = None,
      aspect_ratio: Option[String] = None,
      gravity: Option[String] = None,
      zoom: Option[String] = None,
      x: Option[String] = None,
      y: Option[String] = None,
      format: Option[String] = None,
      fetch_format: Option[String] = None,
      quality: Option[String] = None,
      radius: Option[String] = None,
      angle: Option[String] = None,
      effect: Option[String] = None,
      opacity: Option[String] = None,
      border: Option[String] = None,
      background: Option[String] = None,
      overlay: Option[String] = None,
      underlay: Option[String] = None,
      default_image: Option[String] = None,
      delay: Option[String] = None,
      color: Option[String] = None,
      color_space: Option[String] = None,
      dpr: Option[String] = None,
      page: Option[String] = None,
      density: Option[String] = None,
      flags: Option[String] = None,
      transformation: Option[String] = None
  )

  object CloudinaryImageFormat {
    implicit val encoder: ArgEncoder[CloudinaryImageFormat] =
      new ArgEncoder[CloudinaryImageFormat] {
        override def encode(value: CloudinaryImageFormat): __Value =
          __ObjectValue(
            List(
              "prettyName" -> value.prettyName.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "width" -> value.width.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "height" -> value.height.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "crop" -> value.crop.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "aspect_ratio" -> value.aspect_ratio.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "gravity" -> value.gravity.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "zoom" -> value.zoom.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "x" -> value.x.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "y" -> value.y.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "format" -> value.format.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "fetch_format" -> value.fetch_format.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "quality" -> value.quality.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "radius" -> value.radius.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "angle" -> value.angle.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "effect" -> value.effect.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "opacity" -> value.opacity.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "border" -> value.border.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "background" -> value.background.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "overlay" -> value.overlay.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "underlay" -> value.underlay.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "default_image" -> value.default_image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "delay" -> value.delay.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "color" -> value.color.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "color_space" -> value.color_space.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "dpr" -> value.dpr.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "page" -> value.page.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "density" -> value.density.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "flags" -> value.flags.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "transformation" -> value.transformation.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value))
            )
          )

        override def typeName: String = "CloudinaryImageFormat"
      }
  }

  case class TagWhereInput(
      AND: Option[List[Option[TagWhereInput]]] = None,
      OR: Option[List[Option[TagWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      language: Option[LanguageWhereInput] = None,
      language_is_null: Option[Boolean] = None,
      description: Option[String] = None,
      description_not: Option[String] = None,
      description_contains: Option[String] = None,
      description_not_contains: Option[String] = None,
      description_starts_with: Option[String] = None,
      description_not_starts_with: Option[String] = None,
      description_ends_with: Option[String] = None,
      description_not_ends_with: Option[String] = None,
      description_i: Option[String] = None,
      description_not_i: Option[String] = None,
      description_contains_i: Option[String] = None,
      description_not_contains_i: Option[String] = None,
      description_starts_with_i: Option[String] = None,
      description_not_starts_with_i: Option[String] = None,
      description_ends_with_i: Option[String] = None,
      description_not_ends_with_i: Option[String] = None,
      description_in: Option[List[Option[String]]] = None,
      description_not_in: Option[List[Option[String]]] = None,
      relevance: Option[Int] = None,
      relevance_not: Option[Int] = None,
      relevance_lt: Option[Int] = None,
      relevance_lte: Option[Int] = None,
      relevance_gt: Option[Int] = None,
      relevance_gte: Option[Int] = None,
      relevance_in: Option[List[Option[Int]]] = None,
      relevance_not_in: Option[List[Option[Int]]] = None,
      image: Option[String] = None,
      image_not: Option[String] = None,
      image_in: Option[List[Option[String]]] = None,
      image_not_in: Option[List[Option[String]]] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object TagWhereInput {
    implicit val encoder: ArgEncoder[TagWhereInput] =
      new ArgEncoder[TagWhereInput] {
        override def encode(value: TagWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageWhereInput]].encode(value)
              ),
              "language_is_null" -> value.language_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "description_not" -> value.description_not.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_contains" -> value.description_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_contains" -> value.description_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_starts_with" -> value.description_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_starts_with" -> value.description_not_starts_with
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_ends_with" -> value.description_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_ends_with" -> value.description_not_ends_with
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_i" -> value.description_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "description_not_i" -> value.description_not_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_contains_i" -> value.description_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_contains_i" -> value.description_not_contains_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_starts_with_i" -> value.description_starts_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_not_starts_with_i" -> value.description_not_starts_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_ends_with_i" -> value.description_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_ends_with_i" -> value.description_not_ends_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_in" -> value.description_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "description_not_in" -> value.description_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "relevance" -> value.relevance.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_not" -> value.relevance_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_lt" -> value.relevance_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_lte" -> value.relevance_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_gt" -> value.relevance_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_gte" -> value.relevance_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "relevance_in" -> value.relevance_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[Int]].encode(value)
                        )
                    )
                  )
              ),
              "relevance_not_in" -> value.relevance_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[Int]].encode(value)
                        )
                    )
                  )
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image_not" -> value.image_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image_in" -> value.image_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "image_not_in" -> value.image_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "TagWhereInput"
      }
  }

  case class TagWhereUniqueInput(id: String)

  object TagWhereUniqueInput {
    implicit val encoder: ArgEncoder[TagWhereUniqueInput] =
      new ArgEncoder[TagWhereUniqueInput] {
        override def encode(value: TagWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "TagWhereUniqueInput"
      }
  }

  case class TagUpdateInput(
      name: Option[String] = None,
      language: Option[LanguageRelateToOneInput] = None,
      description: Option[String] = None,
      relevance: Option[Int] = None,
      image: Option[Upload] = None
  )

  object TagUpdateInput {
    implicit val encoder: ArgEncoder[TagUpdateInput] =
      new ArgEncoder[TagUpdateInput] {
        override def encode(value: TagUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "relevance" -> value.relevance.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Upload]].encode(value)
              )
            )
          )

        override def typeName: String = "TagUpdateInput"
      }
  }

  case class TagsUpdateInput(id: String, data: Option[TagUpdateInput] = None)

  object TagsUpdateInput {
    implicit val encoder: ArgEncoder[TagsUpdateInput] =
      new ArgEncoder[TagsUpdateInput] {
        override def encode(value: TagsUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[TagUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "TagsUpdateInput"
      }
  }

  case class TagCreateInput(
      name: Option[String] = None,
      language: Option[LanguageRelateToOneInput] = None,
      description: Option[String] = None,
      relevance: Option[Int] = None,
      image: Option[Upload] = None
  )

  object TagCreateInput {
    implicit val encoder: ArgEncoder[TagCreateInput] =
      new ArgEncoder[TagCreateInput] {
        override def encode(value: TagCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "relevance" -> value.relevance.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Int]].encode(value)
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Upload]].encode(value)
              )
            )
          )

        override def typeName: String = "TagCreateInput"
      }
  }

  case class TagsCreateInput(data: Option[TagCreateInput] = None)

  object TagsCreateInput {
    implicit val encoder: ArgEncoder[TagsCreateInput] =
      new ArgEncoder[TagsCreateInput] {
        override def encode(value: TagsCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[TagCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "TagsCreateInput"
      }
  }

  case class LanguageWhereInput(
      AND: Option[List[Option[LanguageWhereInput]]] = None,
      OR: Option[List[Option[LanguageWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object LanguageWhereInput {
    implicit val encoder: ArgEncoder[LanguageWhereInput] =
      new ArgEncoder[LanguageWhereInput] {
        override def encode(value: LanguageWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "LanguageWhereInput"
      }
  }

  case class LanguageWhereUniqueInput(id: String)

  object LanguageWhereUniqueInput {
    implicit val encoder: ArgEncoder[LanguageWhereUniqueInput] =
      new ArgEncoder[LanguageWhereUniqueInput] {
        override def encode(value: LanguageWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "LanguageWhereUniqueInput"
      }
  }

  case class LanguageUpdateInput(name: Option[String] = None)

  object LanguageUpdateInput {
    implicit val encoder: ArgEncoder[LanguageUpdateInput] =
      new ArgEncoder[LanguageUpdateInput] {
        override def encode(value: LanguageUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "LanguageUpdateInput"
      }
  }

  case class LanguagesUpdateInput(
      id: String,
      data: Option[LanguageUpdateInput] = None
  )

  object LanguagesUpdateInput {
    implicit val encoder: ArgEncoder[LanguagesUpdateInput] =
      new ArgEncoder[LanguagesUpdateInput] {
        override def encode(value: LanguagesUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "LanguagesUpdateInput"
      }
  }

  case class LanguageCreateInput(name: Option[String] = None)

  object LanguageCreateInput {
    implicit val encoder: ArgEncoder[LanguageCreateInput] =
      new ArgEncoder[LanguageCreateInput] {
        override def encode(value: LanguageCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "LanguageCreateInput"
      }
  }

  case class LanguagesCreateInput(data: Option[LanguageCreateInput] = None)

  object LanguagesCreateInput {
    implicit val encoder: ArgEncoder[LanguagesCreateInput] =
      new ArgEncoder[LanguagesCreateInput] {
        override def encode(value: LanguagesCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "LanguagesCreateInput"
      }
  }

  case class GeoLocationWhereInput(
      AND: Option[List[Option[GeoLocationWhereInput]]] = None,
      OR: Option[List[Option[GeoLocationWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      location: Option[String] = None,
      location_not: Option[String] = None,
      location_in: Option[List[Option[String]]] = None,
      location_not_in: Option[List[Option[String]]] = None,
      radius: Option[Double] = None,
      radius_not: Option[Double] = None,
      radius_lt: Option[Double] = None,
      radius_lte: Option[Double] = None,
      radius_gt: Option[Double] = None,
      radius_gte: Option[Double] = None,
      radius_in: Option[List[Option[Double]]] = None,
      radius_not_in: Option[List[Option[Double]]] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object GeoLocationWhereInput {
    implicit val encoder: ArgEncoder[GeoLocationWhereInput] =
      new ArgEncoder[GeoLocationWhereInput] {
        override def encode(value: GeoLocationWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location_not" -> value.location_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location_in" -> value.location_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "location_not_in" -> value.location_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "radius" -> value.radius.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_not" -> value.radius_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_lt" -> value.radius_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_lte" -> value.radius_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_gt" -> value.radius_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_gte" -> value.radius_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              ),
              "radius_in" -> value.radius_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[Double]].encode(value)
                        )
                    )
                  )
              ),
              "radius_not_in" -> value.radius_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[Double]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "GeoLocationWhereInput"
      }
  }

  case class GeoLocationWhereUniqueInput(id: String)

  object GeoLocationWhereUniqueInput {
    implicit val encoder: ArgEncoder[GeoLocationWhereUniqueInput] =
      new ArgEncoder[GeoLocationWhereUniqueInput] {
        override def encode(value: GeoLocationWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "GeoLocationWhereUniqueInput"
      }
  }

  case class GeoLocationUpdateInput(
      name: Option[String] = None,
      location: Option[String] = None,
      radius: Option[Double] = None
  )

  object GeoLocationUpdateInput {
    implicit val encoder: ArgEncoder[GeoLocationUpdateInput] =
      new ArgEncoder[GeoLocationUpdateInput] {
        override def encode(value: GeoLocationUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "radius" -> value.radius.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              )
            )
          )

        override def typeName: String = "GeoLocationUpdateInput"
      }
  }

  case class GeoLocationsUpdateInput(
      id: String,
      data: Option[GeoLocationUpdateInput] = None
  )

  object GeoLocationsUpdateInput {
    implicit val encoder: ArgEncoder[GeoLocationsUpdateInput] =
      new ArgEncoder[GeoLocationsUpdateInput] {
        override def encode(value: GeoLocationsUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "GeoLocationsUpdateInput"
      }
  }

  case class GeoLocationCreateInput(
      name: Option[String] = None,
      location: Option[String] = None,
      radius: Option[Double] = None
  )

  object GeoLocationCreateInput {
    implicit val encoder: ArgEncoder[GeoLocationCreateInput] =
      new ArgEncoder[GeoLocationCreateInput] {
        override def encode(value: GeoLocationCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "radius" -> value.radius.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Double]].encode(value)
              )
            )
          )

        override def typeName: String = "GeoLocationCreateInput"
      }
  }

  case class GeoLocationsCreateInput(
      data: Option[GeoLocationCreateInput] = None
  )

  object GeoLocationsCreateInput {
    implicit val encoder: ArgEncoder[GeoLocationsCreateInput] =
      new ArgEncoder[GeoLocationsCreateInput] {
        override def encode(value: GeoLocationsCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "GeoLocationsCreateInput"
      }
  }

  case class GeoLocationRelateToOneInput(
      create: Option[GeoLocationCreateInput] = None,
      connect: Option[GeoLocationWhereUniqueInput] = None,
      disconnect: Option[GeoLocationWhereUniqueInput] = None,
      disconnectAll: Option[Boolean] = None
  )

  object GeoLocationRelateToOneInput {
    implicit val encoder: ArgEncoder[GeoLocationRelateToOneInput] =
      new ArgEncoder[GeoLocationRelateToOneInput] {
        override def encode(value: GeoLocationRelateToOneInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationCreateInput]].encode(value)
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationWhereUniqueInput]]
                    .encode(value)
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationWhereUniqueInput]]
                    .encode(value)
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "GeoLocationRelateToOneInput"
      }
  }

  case class SourceWhereInput(
      AND: Option[List[Option[SourceWhereInput]]] = None,
      OR: Option[List[Option[SourceWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      url: Option[String] = None,
      url_not: Option[String] = None,
      url_contains: Option[String] = None,
      url_not_contains: Option[String] = None,
      url_starts_with: Option[String] = None,
      url_not_starts_with: Option[String] = None,
      url_ends_with: Option[String] = None,
      url_not_ends_with: Option[String] = None,
      url_i: Option[String] = None,
      url_not_i: Option[String] = None,
      url_contains_i: Option[String] = None,
      url_not_contains_i: Option[String] = None,
      url_starts_with_i: Option[String] = None,
      url_not_starts_with_i: Option[String] = None,
      url_ends_with_i: Option[String] = None,
      url_not_ends_with_i: Option[String] = None,
      url_in: Option[List[Option[String]]] = None,
      url_not_in: Option[List[Option[String]]] = None,
      location: Option[GeoLocationWhereInput] = None,
      location_is_null: Option[Boolean] = None,
      description: Option[String] = None,
      description_not: Option[String] = None,
      description_contains: Option[String] = None,
      description_not_contains: Option[String] = None,
      description_starts_with: Option[String] = None,
      description_not_starts_with: Option[String] = None,
      description_ends_with: Option[String] = None,
      description_not_ends_with: Option[String] = None,
      description_i: Option[String] = None,
      description_not_i: Option[String] = None,
      description_contains_i: Option[String] = None,
      description_not_contains_i: Option[String] = None,
      description_starts_with_i: Option[String] = None,
      description_not_starts_with_i: Option[String] = None,
      description_ends_with_i: Option[String] = None,
      description_not_ends_with_i: Option[String] = None,
      description_in: Option[List[Option[String]]] = None,
      description_not_in: Option[List[Option[String]]] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object SourceWhereInput {
    implicit val encoder: ArgEncoder[SourceWhereInput] =
      new ArgEncoder[SourceWhereInput] {
        override def encode(value: SourceWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not" -> value.url_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_contains" -> value.url_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_contains" -> value.url_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_starts_with" -> value.url_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_starts_with" -> value.url_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_ends_with" -> value.url_ends_with.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_ends_with" -> value.url_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_i" -> value.url_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_i" -> value.url_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_contains_i" -> value.url_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_contains_i" -> value.url_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_starts_with_i" -> value.url_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_starts_with_i" -> value.url_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_ends_with_i" -> value.url_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_ends_with_i" -> value.url_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_in" -> value.url_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "url_not_in" -> value.url_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationWhereInput]].encode(value)
              ),
              "location_is_null" -> value.location_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "description_not" -> value.description_not.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_contains" -> value.description_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_contains" -> value.description_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_starts_with" -> value.description_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_starts_with" -> value.description_not_starts_with
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_ends_with" -> value.description_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_ends_with" -> value.description_not_ends_with
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_i" -> value.description_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "description_not_i" -> value.description_not_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_contains_i" -> value.description_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_contains_i" -> value.description_not_contains_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_starts_with_i" -> value.description_starts_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_not_starts_with_i" -> value.description_not_starts_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_ends_with_i" -> value.description_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "description_not_ends_with_i" -> value.description_not_ends_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "description_in" -> value.description_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "description_not_in" -> value.description_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "SourceWhereInput"
      }
  }

  case class SourceWhereUniqueInput(id: String)

  object SourceWhereUniqueInput {
    implicit val encoder: ArgEncoder[SourceWhereUniqueInput] =
      new ArgEncoder[SourceWhereUniqueInput] {
        override def encode(value: SourceWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "SourceWhereUniqueInput"
      }
  }

  case class SourceUpdateInput(
      name: Option[String] = None,
      url: Option[String] = None,
      location: Option[GeoLocationRelateToOneInput] = None,
      description: Option[String] = None
  )

  object SourceUpdateInput {
    implicit val encoder: ArgEncoder[SourceUpdateInput] =
      new ArgEncoder[SourceUpdateInput] {
        override def encode(value: SourceUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationRelateToOneInput]]
                    .encode(value)
              ),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "SourceUpdateInput"
      }
  }

  case class SourcesUpdateInput(
      id: String,
      data: Option[SourceUpdateInput] = None
  )

  object SourcesUpdateInput {
    implicit val encoder: ArgEncoder[SourcesUpdateInput] =
      new ArgEncoder[SourcesUpdateInput] {
        override def encode(value: SourcesUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "SourcesUpdateInput"
      }
  }

  case class SourceCreateInput(
      name: Option[String] = None,
      url: Option[String] = None,
      location: Option[GeoLocationRelateToOneInput] = None,
      description: Option[String] = None
  )

  object SourceCreateInput {
    implicit val encoder: ArgEncoder[SourceCreateInput] =
      new ArgEncoder[SourceCreateInput] {
        override def encode(value: SourceCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "location" -> value.location.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[GeoLocationRelateToOneInput]]
                    .encode(value)
              ),
              "description" -> value.description.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "SourceCreateInput"
      }
  }

  case class SourcesCreateInput(data: Option[SourceCreateInput] = None)

  object SourcesCreateInput {
    implicit val encoder: ArgEncoder[SourcesCreateInput] =
      new ArgEncoder[SourcesCreateInput] {
        override def encode(value: SourcesCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "SourcesCreateInput"
      }
  }

  case class OrganizationRelateToOneInput(
      create: Option[OrganizationCreateInput] = None,
      connect: Option[OrganizationWhereUniqueInput] = None,
      disconnect: Option[OrganizationWhereUniqueInput] = None,
      disconnectAll: Option[Boolean] = None
  )

  object OrganizationRelateToOneInput {
    implicit val encoder: ArgEncoder[OrganizationRelateToOneInput] =
      new ArgEncoder[OrganizationRelateToOneInput] {
        override def encode(value: OrganizationRelateToOneInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationCreateInput]].encode(value)
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationWhereUniqueInput]]
                    .encode(value)
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationWhereUniqueInput]]
                    .encode(value)
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "OrganizationRelateToOneInput"
      }
  }

  case class SourceRelateToManyInput(
      create: Option[List[Option[SourceCreateInput]]] = None,
      connect: Option[List[Option[SourceWhereUniqueInput]]] = None,
      disconnect: Option[List[Option[SourceWhereUniqueInput]]] = None,
      disconnectAll: Option[Boolean] = None
  )

  object SourceRelateToManyInput {
    implicit val encoder: ArgEncoder[SourceRelateToManyInput] =
      new ArgEncoder[SourceRelateToManyInput] {
        override def encode(value: SourceRelateToManyInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[SourceCreateInput]]
                              .encode(value)
                        )
                    )
                  )
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[SourceWhereUniqueInput]]
                              .encode(value)
                        )
                    )
                  )
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[SourceWhereUniqueInput]]
                              .encode(value)
                        )
                    )
                  )
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "SourceRelateToManyInput"
      }
  }

  case class WidgetWhereInput(
      AND: Option[List[Option[WidgetWhereInput]]] = None,
      OR: Option[List[Option[WidgetWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      organization: Option[OrganizationWhereInput] = None,
      organization_is_null: Option[Boolean] = None,
      language: Option[LanguageWhereInput] = None,
      language_is_null: Option[Boolean] = None,
      sources_every: Option[SourceWhereInput] = None,
      sources_some: Option[SourceWhereInput] = None,
      sources_none: Option[SourceWhereInput] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object WidgetWhereInput {
    implicit val encoder: ArgEncoder[WidgetWhereInput] =
      new ArgEncoder[WidgetWhereInput] {
        override def encode(value: WidgetWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationWhereInput]].encode(value)
              ),
              "organization_is_null" -> value.organization_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageWhereInput]].encode(value)
              ),
              "language_is_null" -> value.language_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "sources_every" -> value.sources_every.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceWhereInput]].encode(value)
              ),
              "sources_some" -> value.sources_some.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceWhereInput]].encode(value)
              ),
              "sources_none" -> value.sources_none.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceWhereInput]].encode(value)
              ),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "WidgetWhereInput"
      }
  }

  case class WidgetWhereUniqueInput(id: String)

  object WidgetWhereUniqueInput {
    implicit val encoder: ArgEncoder[WidgetWhereUniqueInput] =
      new ArgEncoder[WidgetWhereUniqueInput] {
        override def encode(value: WidgetWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "WidgetWhereUniqueInput"
      }
  }

  case class WidgetUpdateInput(
      name: Option[String] = None,
      organization: Option[OrganizationRelateToOneInput] = None,
      language: Option[LanguageRelateToOneInput] = None,
      sources: Option[SourceRelateToManyInput] = None
  )

  object WidgetUpdateInput {
    implicit val encoder: ArgEncoder[WidgetUpdateInput] =
      new ArgEncoder[WidgetUpdateInput] {
        override def encode(value: WidgetUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationRelateToOneInput]]
                    .encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "sources" -> value.sources.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceRelateToManyInput]].encode(value)
              )
            )
          )

        override def typeName: String = "WidgetUpdateInput"
      }
  }

  case class WidgetsUpdateInput(
      id: String,
      data: Option[WidgetUpdateInput] = None
  )

  object WidgetsUpdateInput {
    implicit val encoder: ArgEncoder[WidgetsUpdateInput] =
      new ArgEncoder[WidgetsUpdateInput] {
        override def encode(value: WidgetsUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[WidgetUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "WidgetsUpdateInput"
      }
  }

  case class WidgetCreateInput(
      name: Option[String] = None,
      organization: Option[OrganizationRelateToOneInput] = None,
      language: Option[LanguageRelateToOneInput] = None,
      sources: Option[SourceRelateToManyInput] = None
  )

  object WidgetCreateInput {
    implicit val encoder: ArgEncoder[WidgetCreateInput] =
      new ArgEncoder[WidgetCreateInput] {
        override def encode(value: WidgetCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationRelateToOneInput]]
                    .encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "sources" -> value.sources.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceRelateToManyInput]].encode(value)
              )
            )
          )

        override def typeName: String = "WidgetCreateInput"
      }
  }

  case class WidgetsCreateInput(data: Option[WidgetCreateInput] = None)

  object WidgetsCreateInput {
    implicit val encoder: ArgEncoder[WidgetsCreateInput] =
      new ArgEncoder[WidgetsCreateInput] {
        override def encode(value: WidgetsCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[WidgetCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "WidgetsCreateInput"
      }
  }

  case class TagRelateToManyInput(
      create: Option[List[Option[TagCreateInput]]] = None,
      connect: Option[List[Option[TagWhereUniqueInput]]] = None,
      disconnect: Option[List[Option[TagWhereUniqueInput]]] = None,
      disconnectAll: Option[Boolean] = None
  )

  object TagRelateToManyInput {
    implicit val encoder: ArgEncoder[TagRelateToManyInput] =
      new ArgEncoder[TagRelateToManyInput] {
        override def encode(value: TagRelateToManyInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[TagCreateInput]].encode(value)
                        )
                    )
                  )
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[TagWhereUniqueInput]]
                              .encode(value)
                        )
                    )
                  )
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[TagWhereUniqueInput]]
                              .encode(value)
                        )
                    )
                  )
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "TagRelateToManyInput"
      }
  }

  case class SourceRelateToOneInput(
      create: Option[SourceCreateInput] = None,
      connect: Option[SourceWhereUniqueInput] = None,
      disconnect: Option[SourceWhereUniqueInput] = None,
      disconnectAll: Option[Boolean] = None
  )

  object SourceRelateToOneInput {
    implicit val encoder: ArgEncoder[SourceRelateToOneInput] =
      new ArgEncoder[SourceRelateToOneInput] {
        override def encode(value: SourceRelateToOneInput): __Value =
          __ObjectValue(
            List(
              "create" -> value.create.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceCreateInput]].encode(value)
              ),
              "connect" -> value.connect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceWhereUniqueInput]].encode(value)
              ),
              "disconnect" -> value.disconnect.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceWhereUniqueInput]].encode(value)
              ),
              "disconnectAll" -> value.disconnectAll.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "SourceRelateToOneInput"
      }
  }

  case class EntryWhereInput(
      AND: Option[List[Option[EntryWhereInput]]] = None,
      OR: Option[List[Option[EntryWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      publishDate: Option[String] = None,
      publishDate_not: Option[String] = None,
      publishDate_lt: Option[String] = None,
      publishDate_lte: Option[String] = None,
      publishDate_gt: Option[String] = None,
      publishDate_gte: Option[String] = None,
      publishDate_in: Option[List[Option[String]]] = None,
      publishDate_not_in: Option[List[Option[String]]] = None,
      title: Option[String] = None,
      title_not: Option[String] = None,
      title_contains: Option[String] = None,
      title_not_contains: Option[String] = None,
      title_starts_with: Option[String] = None,
      title_not_starts_with: Option[String] = None,
      title_ends_with: Option[String] = None,
      title_not_ends_with: Option[String] = None,
      title_i: Option[String] = None,
      title_not_i: Option[String] = None,
      title_contains_i: Option[String] = None,
      title_not_contains_i: Option[String] = None,
      title_starts_with_i: Option[String] = None,
      title_not_starts_with_i: Option[String] = None,
      title_ends_with_i: Option[String] = None,
      title_not_ends_with_i: Option[String] = None,
      title_in: Option[List[Option[String]]] = None,
      title_not_in: Option[List[Option[String]]] = None,
      image: Option[String] = None,
      image_not: Option[String] = None,
      image_in: Option[List[Option[String]]] = None,
      image_not_in: Option[List[Option[String]]] = None,
      content: Option[String] = None,
      content_not: Option[String] = None,
      content_contains: Option[String] = None,
      content_not_contains: Option[String] = None,
      content_starts_with: Option[String] = None,
      content_not_starts_with: Option[String] = None,
      content_ends_with: Option[String] = None,
      content_not_ends_with: Option[String] = None,
      content_i: Option[String] = None,
      content_not_i: Option[String] = None,
      content_contains_i: Option[String] = None,
      content_not_contains_i: Option[String] = None,
      content_starts_with_i: Option[String] = None,
      content_not_starts_with_i: Option[String] = None,
      content_ends_with_i: Option[String] = None,
      content_not_ends_with_i: Option[String] = None,
      content_in: Option[List[Option[String]]] = None,
      content_not_in: Option[List[Option[String]]] = None,
      url: Option[String] = None,
      url_not: Option[String] = None,
      url_contains: Option[String] = None,
      url_not_contains: Option[String] = None,
      url_starts_with: Option[String] = None,
      url_not_starts_with: Option[String] = None,
      url_ends_with: Option[String] = None,
      url_not_ends_with: Option[String] = None,
      url_i: Option[String] = None,
      url_not_i: Option[String] = None,
      url_contains_i: Option[String] = None,
      url_not_contains_i: Option[String] = None,
      url_starts_with_i: Option[String] = None,
      url_not_starts_with_i: Option[String] = None,
      url_ends_with_i: Option[String] = None,
      url_not_ends_with_i: Option[String] = None,
      url_in: Option[List[Option[String]]] = None,
      url_not_in: Option[List[Option[String]]] = None,
      tags_every: Option[TagWhereInput] = None,
      tags_some: Option[TagWhereInput] = None,
      tags_none: Option[TagWhereInput] = None,
      language: Option[LanguageWhereInput] = None,
      language_is_null: Option[Boolean] = None,
      source: Option[SourceWhereInput] = None,
      source_is_null: Option[Boolean] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object EntryWhereInput {
    implicit val encoder: ArgEncoder[EntryWhereInput] =
      new ArgEncoder[EntryWhereInput] {
        override def encode(value: EntryWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "publishDate" -> value.publishDate.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "publishDate_not" -> value.publishDate_not.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "publishDate_lt" -> value.publishDate_lt.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "publishDate_lte" -> value.publishDate_lte.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "publishDate_gt" -> value.publishDate_gt.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "publishDate_gte" -> value.publishDate_gte.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "publishDate_in" -> value.publishDate_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "publishDate_not_in" -> value.publishDate_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "title" -> value.title.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title_not" -> value.title_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title_contains" -> value.title_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_contains" -> value.title_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_starts_with" -> value.title_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_starts_with" -> value.title_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_ends_with" -> value.title_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_ends_with" -> value.title_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_i" -> value.title_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title_not_i" -> value.title_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title_contains_i" -> value.title_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_contains_i" -> value.title_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_starts_with_i" -> value.title_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_starts_with_i" -> value.title_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_ends_with_i" -> value.title_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_not_ends_with_i" -> value.title_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "title_in" -> value.title_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "title_not_in" -> value.title_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image_not" -> value.image_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image_in" -> value.image_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "image_not_in" -> value.image_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "content" -> value.content.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "content_not" -> value.content_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "content_contains" -> value.content_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_contains" -> value.content_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_starts_with" -> value.content_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_starts_with" -> value.content_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_ends_with" -> value.content_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_ends_with" -> value.content_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_i" -> value.content_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "content_not_i" -> value.content_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "content_contains_i" -> value.content_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_contains_i" -> value.content_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_starts_with_i" -> value.content_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_starts_with_i" -> value.content_not_starts_with_i
                .fold(__NullValue: __Value)(
                  value => implicitly[ArgEncoder[String]].encode(value)
                ),
              "content_ends_with_i" -> value.content_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_not_ends_with_i" -> value.content_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "content_in" -> value.content_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "content_not_in" -> value.content_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not" -> value.url_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_contains" -> value.url_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_contains" -> value.url_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_starts_with" -> value.url_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_starts_with" -> value.url_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_ends_with" -> value.url_ends_with.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_ends_with" -> value.url_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_i" -> value.url_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_not_i" -> value.url_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url_contains_i" -> value.url_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_contains_i" -> value.url_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_starts_with_i" -> value.url_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_starts_with_i" -> value.url_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_ends_with_i" -> value.url_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_not_ends_with_i" -> value.url_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "url_in" -> value.url_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "url_not_in" -> value.url_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "tags_every" -> value.tags_every.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[TagWhereInput]].encode(value)
              ),
              "tags_some" -> value.tags_some.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[TagWhereInput]].encode(value)
              ),
              "tags_none" -> value.tags_none.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[TagWhereInput]].encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageWhereInput]].encode(value)
              ),
              "language_is_null" -> value.language_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "source" -> value.source.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[SourceWhereInput]].encode(value)
              ),
              "source_is_null" -> value.source_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "EntryWhereInput"
      }
  }

  case class EntryWhereUniqueInput(id: String)

  object EntryWhereUniqueInput {
    implicit val encoder: ArgEncoder[EntryWhereUniqueInput] =
      new ArgEncoder[EntryWhereUniqueInput] {
        override def encode(value: EntryWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "EntryWhereUniqueInput"
      }
  }

  case class EntryUpdateInput(
      publishDate: Option[String] = None,
      title: Option[String] = None,
      image: Option[Upload] = None,
      content: Option[String] = None,
      url: Option[String] = None,
      tags: Option[TagRelateToManyInput] = None,
      language: Option[LanguageRelateToOneInput] = None,
      source: Option[SourceRelateToOneInput] = None
  )

  object EntryUpdateInput {
    implicit val encoder: ArgEncoder[EntryUpdateInput] =
      new ArgEncoder[EntryUpdateInput] {
        override def encode(value: EntryUpdateInput): __Value =
          __ObjectValue(
            List(
              "publishDate" -> value.publishDate.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title" -> value.title.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Upload]].encode(value)
              ),
              "content" -> value.content.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "tags" -> value.tags.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[TagRelateToManyInput]].encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "source" -> value.source.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceRelateToOneInput]].encode(value)
              )
            )
          )

        override def typeName: String = "EntryUpdateInput"
      }
  }

  case class EntriesUpdateInput(
      id: String,
      data: Option[EntryUpdateInput] = None
  )

  object EntriesUpdateInput {
    implicit val encoder: ArgEncoder[EntriesUpdateInput] =
      new ArgEncoder[EntriesUpdateInput] {
        override def encode(value: EntriesUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[EntryUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "EntriesUpdateInput"
      }
  }

  case class EntryCreateInput(
      publishDate: Option[String] = None,
      title: Option[String] = None,
      image: Option[Upload] = None,
      content: Option[String] = None,
      url: Option[String] = None,
      tags: Option[TagRelateToManyInput] = None,
      language: Option[LanguageRelateToOneInput] = None,
      source: Option[SourceRelateToOneInput] = None
  )

  object EntryCreateInput {
    implicit val encoder: ArgEncoder[EntryCreateInput] =
      new ArgEncoder[EntryCreateInput] {
        override def encode(value: EntryCreateInput): __Value =
          __ObjectValue(
            List(
              "publishDate" -> value.publishDate.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "title" -> value.title.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "image" -> value.image.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Upload]].encode(value)
              ),
              "content" -> value.content.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "url" -> value.url.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "tags" -> value.tags.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[TagRelateToManyInput]].encode(value)
              ),
              "language" -> value.language.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[LanguageRelateToOneInput]].encode(value)
              ),
              "source" -> value.source.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[SourceRelateToOneInput]].encode(value)
              )
            )
          )

        override def typeName: String = "EntryCreateInput"
      }
  }

  case class EntriesCreateInput(data: Option[EntryCreateInput] = None)

  object EntriesCreateInput {
    implicit val encoder: ArgEncoder[EntriesCreateInput] =
      new ArgEncoder[EntriesCreateInput] {
        override def encode(value: EntriesCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[EntryCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "EntriesCreateInput"
      }
  }

  case class UserWhereInput(
      AND: Option[List[Option[UserWhereInput]]] = None,
      OR: Option[List[Option[UserWhereInput]]] = None,
      id: Option[String] = None,
      id_not: Option[String] = None,
      id_in: Option[List[Option[String]]] = None,
      id_not_in: Option[List[Option[String]]] = None,
      name: Option[String] = None,
      name_not: Option[String] = None,
      name_contains: Option[String] = None,
      name_not_contains: Option[String] = None,
      name_starts_with: Option[String] = None,
      name_not_starts_with: Option[String] = None,
      name_ends_with: Option[String] = None,
      name_not_ends_with: Option[String] = None,
      name_i: Option[String] = None,
      name_not_i: Option[String] = None,
      name_contains_i: Option[String] = None,
      name_not_contains_i: Option[String] = None,
      name_starts_with_i: Option[String] = None,
      name_not_starts_with_i: Option[String] = None,
      name_ends_with_i: Option[String] = None,
      name_not_ends_with_i: Option[String] = None,
      name_in: Option[List[Option[String]]] = None,
      name_not_in: Option[List[Option[String]]] = None,
      email: Option[String] = None,
      email_not: Option[String] = None,
      email_contains: Option[String] = None,
      email_not_contains: Option[String] = None,
      email_starts_with: Option[String] = None,
      email_not_starts_with: Option[String] = None,
      email_ends_with: Option[String] = None,
      email_not_ends_with: Option[String] = None,
      email_i: Option[String] = None,
      email_not_i: Option[String] = None,
      email_contains_i: Option[String] = None,
      email_not_contains_i: Option[String] = None,
      email_starts_with_i: Option[String] = None,
      email_not_starts_with_i: Option[String] = None,
      email_ends_with_i: Option[String] = None,
      email_not_ends_with_i: Option[String] = None,
      email_in: Option[List[Option[String]]] = None,
      email_not_in: Option[List[Option[String]]] = None,
      isAdmin: Option[Boolean] = None,
      isAdmin_not: Option[Boolean] = None,
      password_is_set: Option[Boolean] = None,
      organization: Option[OrganizationWhereInput] = None,
      organization_is_null: Option[Boolean] = None,
      updatedAt: Option[DateTime] = None,
      updatedAt_not: Option[DateTime] = None,
      updatedAt_lt: Option[DateTime] = None,
      updatedAt_lte: Option[DateTime] = None,
      updatedAt_gt: Option[DateTime] = None,
      updatedAt_gte: Option[DateTime] = None,
      updatedAt_in: Option[List[Option[DateTime]]] = None,
      updatedAt_not_in: Option[List[Option[DateTime]]] = None,
      createdAt: Option[DateTime] = None,
      createdAt_not: Option[DateTime] = None,
      createdAt_lt: Option[DateTime] = None,
      createdAt_lte: Option[DateTime] = None,
      createdAt_gt: Option[DateTime] = None,
      createdAt_gte: Option[DateTime] = None,
      createdAt_in: Option[List[Option[DateTime]]] = None,
      createdAt_not_in: Option[List[Option[DateTime]]] = None
  )

  object UserWhereInput {
    implicit val encoder: ArgEncoder[UserWhereInput] =
      new ArgEncoder[UserWhereInput] {
        override def encode(value: UserWhereInput): __Value =
          __ObjectValue(
            List(
              "AND" -> value.AND.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "OR" -> value.OR.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(value => encode(value))
                    )
                  )
              ),
              "id" -> value.id.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_not" -> value.id_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "id_in" -> value.id_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "id_not_in" -> value.id_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not" -> value.name_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains" -> value.name_contains.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_contains" -> value.name_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with" -> value.name_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with" -> value.name_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with" -> value.name_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with" -> value.name_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_i" -> value.name_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_not_i" -> value.name_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "name_contains_i" -> value.name_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_contains_i" -> value.name_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_starts_with_i" -> value.name_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_starts_with_i" -> value.name_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_ends_with_i" -> value.name_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_not_ends_with_i" -> value.name_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "name_in" -> value.name_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "name_not_in" -> value.name_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "email" -> value.email.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email_not" -> value.email_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email_contains" -> value.email_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_contains" -> value.email_not_contains.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_starts_with" -> value.email_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_starts_with" -> value.email_not_starts_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_ends_with" -> value.email_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_ends_with" -> value.email_not_ends_with.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_i" -> value.email_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email_not_i" -> value.email_not_i.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email_contains_i" -> value.email_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_contains_i" -> value.email_not_contains_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_starts_with_i" -> value.email_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_starts_with_i" -> value.email_not_starts_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_ends_with_i" -> value.email_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_not_ends_with_i" -> value.email_not_ends_with_i.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[String]].encode(value)),
              "email_in" -> value.email_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "email_not_in" -> value.email_not_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value => implicitly[ArgEncoder[String]].encode(value)
                        )
                    )
                  )
              ),
              "isAdmin" -> value.isAdmin.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              ),
              "isAdmin_not" -> value.isAdmin_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              ),
              "password_is_set" -> value.password_is_set.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationWhereInput]].encode(value)
              ),
              "organization_is_null" -> value.organization_is_null.fold(
                __NullValue: __Value
              )(value => implicitly[ArgEncoder[Boolean]].encode(value)),
              "updatedAt" -> value.updatedAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_not" -> value.updatedAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lt" -> value.updatedAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_lte" -> value.updatedAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gt" -> value.updatedAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_gte" -> value.updatedAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "updatedAt_in" -> value.updatedAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "updatedAt_not_in" -> value.updatedAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt" -> value.createdAt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_not" -> value.createdAt_not.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lt" -> value.createdAt_lt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_lte" -> value.createdAt_lte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gt" -> value.createdAt_gt.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_gte" -> value.createdAt_gte.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[DateTime]].encode(value)
              ),
              "createdAt_in" -> value.createdAt_in.fold(__NullValue: __Value)(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              ),
              "createdAt_not_in" -> value.createdAt_not_in.fold(
                __NullValue: __Value
              )(
                value =>
                  __ListValue(
                    value.map(
                      value =>
                        value.fold(__NullValue: __Value)(
                          value =>
                            implicitly[ArgEncoder[DateTime]].encode(value)
                        )
                    )
                  )
              )
            )
          )

        override def typeName: String = "UserWhereInput"
      }
  }

  case class UserWhereUniqueInput(id: String)

  object UserWhereUniqueInput {
    implicit val encoder: ArgEncoder[UserWhereUniqueInput] =
      new ArgEncoder[UserWhereUniqueInput] {
        override def encode(value: UserWhereUniqueInput): __Value =
          __ObjectValue(
            List("id" -> implicitly[ArgEncoder[String]].encode(value.id))
          )

        override def typeName: String = "UserWhereUniqueInput"
      }
  }

  case class UserUpdateInput(
      name: Option[String] = None,
      email: Option[String] = None,
      isAdmin: Option[Boolean] = None,
      password: Option[String] = None,
      organization: Option[OrganizationRelateToOneInput] = None
  )

  object UserUpdateInput {
    implicit val encoder: ArgEncoder[UserUpdateInput] =
      new ArgEncoder[UserUpdateInput] {
        override def encode(value: UserUpdateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email" -> value.email.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "isAdmin" -> value.isAdmin.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              ),
              "password" -> value.password.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationRelateToOneInput]].encode(
                    value
                  )
              )
            )
          )

        override def typeName: String = "UserUpdateInput"
      }
  }

  case class UsersUpdateInput(id: String, data: Option[UserUpdateInput] = None)

  object UsersUpdateInput {
    implicit val encoder: ArgEncoder[UsersUpdateInput] =
      new ArgEncoder[UsersUpdateInput] {
        override def encode(value: UsersUpdateInput): __Value =
          __ObjectValue(
            List(
              "id" -> implicitly[ArgEncoder[String]].encode(value.id),
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[UserUpdateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "UsersUpdateInput"
      }
  }

  case class UserCreateInput(
      name: Option[String] = None,
      email: Option[String] = None,
      isAdmin: Option[Boolean] = None,
      password: Option[String] = None,
      organization: Option[OrganizationRelateToOneInput] = None
  )

  object UserCreateInput {
    implicit val encoder: ArgEncoder[UserCreateInput] =
      new ArgEncoder[UserCreateInput] {
        override def encode(value: UserCreateInput): __Value =
          __ObjectValue(
            List(
              "name" -> value.name.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "email" -> value.email.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "isAdmin" -> value.isAdmin.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              ),
              "password" -> value.password.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "organization" -> value.organization.fold(__NullValue: __Value)(
                value =>
                  implicitly[ArgEncoder[OrganizationRelateToOneInput]].encode(
                    value
                  )
              )
            )
          )

        override def typeName: String = "UserCreateInput"
      }
  }

  case class UsersCreateInput(data: Option[UserCreateInput] = None)

  object UsersCreateInput {
    implicit val encoder: ArgEncoder[UsersCreateInput] =
      new ArgEncoder[UsersCreateInput] {
        override def encode(value: UsersCreateInput): __Value =
          __ObjectValue(
            List(
              "data" -> value.data.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[UserCreateInput]].encode(value)
              )
            )
          )

        override def typeName: String = "UsersCreateInput"
      }
  }

  case class _ksListsMetaInput(
      key: Option[String] = None,
      auxiliary: Option[Boolean] = None
  )

  object _ksListsMetaInput {
    implicit val encoder: ArgEncoder[_ksListsMetaInput] =
      new ArgEncoder[_ksListsMetaInput] {
        override def encode(value: _ksListsMetaInput): __Value =
          __ObjectValue(
            List(
              "key" -> value.key.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              ),
              "auxiliary" -> value.auxiliary.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[Boolean]].encode(value)
              )
            )
          )

        override def typeName: String = "_ksListsMetaInput"
      }
  }

  case class _ListSchemaFieldsInput(`type`: Option[String] = None)

  object _ListSchemaFieldsInput {
    implicit val encoder: ArgEncoder[_ListSchemaFieldsInput] =
      new ArgEncoder[_ListSchemaFieldsInput] {
        override def encode(value: _ListSchemaFieldsInput): __Value =
          __ObjectValue(
            List(
              "type" -> value.`type`.fold(__NullValue: __Value)(
                value => implicitly[ArgEncoder[String]].encode(value)
              )
            )
          )

        override def typeName: String = "_ListSchemaFieldsInput"
      }
  }

  type Query = RootQuery

  object Query {

    /** Search for all Organization items which match the where clause.
      */
    def allOrganizations[A](
        where: Option[OrganizationWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortOrganizationsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allOrganizations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Organization item with the matching ID.
      */
    def Organization[A](where: OrganizationWhereUniqueInput)(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Organization",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Organization items which match the where clause.
      */
    def _allOrganizationsMeta[A](
        where: Option[OrganizationWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortOrganizationsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allOrganizationsMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Organization list.
      */
    def _OrganizationsMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_OrganizationsMeta", OptionOf(Obj(innerSelection)))

    /** Search for all Tag items which match the where clause.
      */
    def allTags[A](
        where: Option[TagWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortTagsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allTags",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Tag item with the matching ID.
      */
    def Tag[A](where: TagWhereUniqueInput)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Tag",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Tag items which match the where clause.
      */
    def _allTagsMeta[A](
        where: Option[TagWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortTagsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allTagsMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Tag list.
      */
    def _TagsMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_TagsMeta", OptionOf(Obj(innerSelection)))

    /** Search for all Language items which match the where clause.
      */
    def allLanguages[A](
        where: Option[LanguageWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortLanguagesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allLanguages",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Language item with the matching ID.
      */
    def Language[A](where: LanguageWhereUniqueInput)(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Language",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Language items which match the where clause.
      */
    def _allLanguagesMeta[A](
        where: Option[LanguageWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortLanguagesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allLanguagesMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Language list.
      */
    def _LanguagesMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_LanguagesMeta", OptionOf(Obj(innerSelection)))

    /** Search for all GeoLocation items which match the where clause.
      */
    def allGeoLocations[A](
        where: Option[GeoLocationWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortGeoLocationsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allGeoLocations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the GeoLocation item with the matching ID.
      */
    def GeoLocation[A](where: GeoLocationWhereUniqueInput)(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "GeoLocation",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all GeoLocation items which match the where clause.
      */
    def _allGeoLocationsMeta[A](
        where: Option[GeoLocationWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortGeoLocationsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allGeoLocationsMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the GeoLocation list.
      */
    def _GeoLocationsMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_GeoLocationsMeta", OptionOf(Obj(innerSelection)))

    /** Search for all Source items which match the where clause.
      */
    def allSources[A](
        where: Option[SourceWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortSourcesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allSources",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Source item with the matching ID.
      */
    def Source[A](where: SourceWhereUniqueInput)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Source",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Source items which match the where clause.
      */
    def _allSourcesMeta[A](
        where: Option[SourceWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortSourcesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allSourcesMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Source list.
      */
    def _SourcesMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_SourcesMeta", OptionOf(Obj(innerSelection)))

    /** Search for all Widget items which match the where clause.
      */
    def allWidgets[A](
        where: Option[WidgetWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortWidgetsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allWidgets",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Widget item with the matching ID.
      */
    def Widget[A](where: WidgetWhereUniqueInput)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Widget",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Widget items which match the where clause.
      */
    def _allWidgetsMeta[A](
        where: Option[WidgetWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortWidgetsBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allWidgetsMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Widget list.
      */
    def _WidgetsMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_WidgetsMeta", OptionOf(Obj(innerSelection)))

    /** Search for all Entry items which match the where clause.
      */
    def allEntries[A](
        where: Option[EntryWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortEntriesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allEntries",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the Entry item with the matching ID.
      */
    def Entry[A](where: EntryWhereUniqueInput)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "Entry",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all Entry items which match the where clause.
      */
    def _allEntriesMeta[A](
        where: Option[EntryWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortEntriesBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allEntriesMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the Entry list.
      */
    def _EntriesMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_EntriesMeta", OptionOf(Obj(innerSelection)))

    /** Search for all User items which match the where clause.
      */
    def allUsers[A](
        where: Option[UserWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortUsersBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "allUsers",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Search for the User item with the matching ID.
      */
    def User[A](where: UserWhereUniqueInput)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "User",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("where", where))
    )

    /** Perform a meta-query on all User items which match the where clause.
      */
    def _allUsersMeta[A](
        where: Option[UserWhereInput] = None,
        search: Option[String] = None,
        sortBy: Option[List[SortUsersBy]] = None,
        orderBy: Option[String] = None,
        first: Option[Int] = None,
        skip: Option[Int] = None
    )(
        innerSelection: SelectionBuilder[_QueryMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] = Field(
      "_allUsersMeta",
      OptionOf(Obj(innerSelection)),
      arguments = List(
        Argument("where", where),
        Argument("search", search),
        Argument("sortBy", sortBy),
        Argument("orderBy", orderBy),
        Argument("first", first),
        Argument("skip", skip)
      )
    )

    /** Retrieve the meta-data for the User list.
      */
    def _UsersMeta[A](
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("_UsersMeta", OptionOf(Obj(innerSelection)))

    /** Retrieve the meta-data for all lists.
      */
    def _ksListsMeta[A](where: Option[_ksListsMetaInput] = None)(
        innerSelection: SelectionBuilder[_ListMeta, A]
    ): SelectionBuilder[RootQuery, Option[List[Option[A]]]] = Field(
      "_ksListsMeta",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("where", where))
    )

    /** The version of the Keystone application serving this API.
      */
    def appVersion: SelectionBuilder[RootQuery, Option[String]] =
      Field("appVersion", OptionOf(Scalar()))

    def authenticatedUser[A](
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootQuery, Option[A]] =
      Field("authenticatedUser", OptionOf(Obj(innerSelection)))
  }

  type Mutation = RootMutation

  object Mutation {

    /** Create a single Organization item.
      */
    def createOrganization[A](data: Option[OrganizationCreateInput] = None)(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createOrganization",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Organization items.
      */
    def createOrganizations[A](
        data: Option[List[Option[OrganizationsCreateInput]]] = None
    )(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createOrganizations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Organization item by ID.
      */
    def updateOrganization[A](
        id: String,
        data: Option[OrganizationUpdateInput] = None
    )(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateOrganization",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Organization items by ID.
      */
    def updateOrganizations[A](
        data: Option[List[Option[OrganizationsUpdateInput]]] = None
    )(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateOrganizations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Organization item by ID.
      */
    def deleteOrganization[A](id: String)(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteOrganization",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Organization items by ID.
      */
    def deleteOrganizations[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Organization, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteOrganizations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single Tag item.
      */
    def createTag[A](data: Option[TagCreateInput] = None)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createTag",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Tag items.
      */
    def createTags[A](data: Option[List[Option[TagsCreateInput]]] = None)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createTags",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Tag item by ID.
      */
    def updateTag[A](id: String, data: Option[TagUpdateInput] = None)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateTag",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Tag items by ID.
      */
    def updateTags[A](data: Option[List[Option[TagsUpdateInput]]] = None)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateTags",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Tag item by ID.
      */
    def deleteTag[A](id: String)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteTag",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Tag items by ID.
      */
    def deleteTags[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Tag, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteTags",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single Language item.
      */
    def createLanguage[A](data: Option[LanguageCreateInput] = None)(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createLanguage",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Language items.
      */
    def createLanguages[A](
        data: Option[List[Option[LanguagesCreateInput]]] = None
    )(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createLanguages",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Language item by ID.
      */
    def updateLanguage[A](id: String, data: Option[LanguageUpdateInput] = None)(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateLanguage",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Language items by ID.
      */
    def updateLanguages[A](
        data: Option[List[Option[LanguagesUpdateInput]]] = None
    )(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateLanguages",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Language item by ID.
      */
    def deleteLanguage[A](id: String)(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteLanguage",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Language items by ID.
      */
    def deleteLanguages[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Language, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteLanguages",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single GeoLocation item.
      */
    def createGeoLocation[A](data: Option[GeoLocationCreateInput] = None)(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createGeoLocation",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple GeoLocation items.
      */
    def createGeoLocations[A](
        data: Option[List[Option[GeoLocationsCreateInput]]] = None
    )(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createGeoLocations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single GeoLocation item by ID.
      */
    def updateGeoLocation[A](
        id: String,
        data: Option[GeoLocationUpdateInput] = None
    )(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateGeoLocation",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple GeoLocation items by ID.
      */
    def updateGeoLocations[A](
        data: Option[List[Option[GeoLocationsUpdateInput]]] = None
    )(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateGeoLocations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single GeoLocation item by ID.
      */
    def deleteGeoLocation[A](id: String)(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteGeoLocation",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple GeoLocation items by ID.
      */
    def deleteGeoLocations[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[GeoLocation, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteGeoLocations",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single Source item.
      */
    def createSource[A](data: Option[SourceCreateInput] = None)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createSource",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Source items.
      */
    def createSources[A](data: Option[List[Option[SourcesCreateInput]]] = None)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createSources",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Source item by ID.
      */
    def updateSource[A](id: String, data: Option[SourceUpdateInput] = None)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateSource",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Source items by ID.
      */
    def updateSources[A](data: Option[List[Option[SourcesUpdateInput]]] = None)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateSources",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Source item by ID.
      */
    def deleteSource[A](id: String)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteSource",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Source items by ID.
      */
    def deleteSources[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Source, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteSources",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single Widget item.
      */
    def createWidget[A](data: Option[WidgetCreateInput] = None)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createWidget",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Widget items.
      */
    def createWidgets[A](data: Option[List[Option[WidgetsCreateInput]]] = None)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createWidgets",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Widget item by ID.
      */
    def updateWidget[A](id: String, data: Option[WidgetUpdateInput] = None)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateWidget",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Widget items by ID.
      */
    def updateWidgets[A](data: Option[List[Option[WidgetsUpdateInput]]] = None)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateWidgets",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Widget item by ID.
      */
    def deleteWidget[A](id: String)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteWidget",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Widget items by ID.
      */
    def deleteWidgets[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Widget, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteWidgets",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single Entry item.
      */
    def createEntry[A](data: Option[EntryCreateInput] = None)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createEntry",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple Entry items.
      */
    def createEntries[A](data: Option[List[Option[EntriesCreateInput]]] = None)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createEntries",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single Entry item by ID.
      */
    def updateEntry[A](id: String, data: Option[EntryUpdateInput] = None)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateEntry",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple Entry items by ID.
      */
    def updateEntries[A](data: Option[List[Option[EntriesUpdateInput]]] = None)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateEntries",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single Entry item by ID.
      */
    def deleteEntry[A](id: String)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteEntry",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple Entry items by ID.
      */
    def deleteEntries[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[Entry, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteEntries",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Create a single User item.
      */
    def createUser[A](data: Option[UserCreateInput] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "createUser",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )

    /** Create multiple User items.
      */
    def createUsers[A](data: Option[List[Option[UsersCreateInput]]] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "createUsers",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Update a single User item by ID.
      */
    def updateUser[A](id: String, data: Option[UserUpdateInput] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateUser",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id), Argument("data", data))
    )

    /** Update multiple User items by ID.
      */
    def updateUsers[A](data: Option[List[Option[UsersUpdateInput]]] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "updateUsers",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("data", data))
    )

    /** Delete a single User item by ID.
      */
    def deleteUser[A](id: String)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "deleteUser",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("id", id))
    )

    /** Delete multiple User items by ID.
      */
    def deleteUsers[A](ids: Option[List[String]] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[List[Option[A]]]] = Field(
      "deleteUsers",
      OptionOf(ListOf(OptionOf(Obj(innerSelection)))),
      arguments = List(Argument("ids", ids))
    )

    /** Authenticate and generate a token for a User with the Password Authentication Strategy.
      */
    def authenticateUserWithPassword[A](
        email: Option[String] = None,
        password: Option[String] = None
    )(
        innerSelection: SelectionBuilder[authenticateUserOutput, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "authenticateUserWithPassword",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("email", email), Argument("password", password))
    )

    def unauthenticateUser[A](
        innerSelection: SelectionBuilder[unauthenticateUserOutput, A]
    ): SelectionBuilder[RootMutation, Option[A]] =
      Field("unauthenticateUser", OptionOf(Obj(innerSelection)))

    def updateAuthenticatedUser[A](data: Option[UserUpdateInput] = None)(
        innerSelection: SelectionBuilder[User, A]
    ): SelectionBuilder[RootMutation, Option[A]] = Field(
      "updateAuthenticatedUser",
      OptionOf(Obj(innerSelection)),
      arguments = List(Argument("data", data))
    )
  }

}
