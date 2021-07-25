/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.scaladsl.Behaviors
import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder
import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import info.coverified.extractor.messages.MutatorMessage
import info.coverified.extractor.messages.MutatorMessage.{
  CreateEntry,
  InitMutator,
  Terminate,
  UpdateUrl
}
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.Tag.TagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  EntryCreateInput,
  Mutation,
  Tag,
  TagCreateInput,
  TagRelateToManyInput,
  TagWhereUniqueInput,
  UrlRelateToOneInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.{
  CoVerifiedClientSchema,
  SimpleEntry,
  SimpleUrl
}
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView

import java.time.{Duration, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

/**
  * This actor takes care of facing all mutation to the GraphQL API
  */
object Mutator {
  def apply(): Behaviors.Receive[MutatorMessage] = uninitialized

  def uninitialized: Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (_, InitMutator(apiUri, authToken, reAnalysisInterval)) =>
        val helper = new GraphQLHelper(apiUri, authToken)
        idle(helper, reAnalysisInterval)
      case _ => Behaviors.unhandled
    }

  def idle(
      helper: GraphQLHelper,
      reAnalysisInterval: Duration
  ): Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (context, CreateEntry(createEntryInformation, urlId, replyTo)) =>
        context.log.debug(
          "Attempting to create a new entry. Check if there is one with same content."
        )
        /* Check, if there isn't yet an entry with the same content */
        val contentHash = createEntryInformation.contentHash.toString
        val disabled = !helper.entriesWithSameHash(contentHash)
        if (disabled) {
          context.log.warn(
            s"There is / are already entries available with the same content hash code. Create an entry, but disable it."
          )
        }

        val mutation = createEntryInformation match {
          case CreateEntryInformation(title, summary, content, date, tags) =>
            buildEntryConsideringExistingStuff(
              urlId,
              title,
              summary,
              content,
              date,
              tags,
              contentHash,
              disabled,
              reAnalysisInterval,
              helper
            )
        }

        helper.saveEntry(mutation) match {
          case Some(_) =>
            context.log.debug("Entry successfully stored.")
          case None =>
          /* TODO: Report to source handler */
        }
        Behaviors.same
      case (context, UpdateUrl(urlId, replyTo)) =>
        context.log.debug(
          "Attempting to update the url with id '{}'.",
          urlId
        )

        helper.updateUrl(urlId) match {
          case Some(_) =>
            context.log.debug(s"Updated url '$urlId'")
          case None =>
          // todo report to source handler
        }

        Behaviors.same
      case (ctx, Terminate) =>
        ctx.log.info("Shutting down mutator!")
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  private def buildEntryConsideringExistingStuff(
      urlId: String,
      title: String,
      summary: Option[String],
      content: Option[String],
      date: Option[String],
      maybeTags: Option[List[String]],
      contentHash: String,
      disabled: Boolean,
      timeToNextCrawl: Duration,
      graphQlHelper: GraphQLHelper
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntryView[SimpleUrlView, TagView[String]]
  ]] = {
    /* Figure out, which tags need to be written */
    val maybeConnectToAndCreateTags =
      maybeTags.map(connectToOrCreateTag(_, graphQlHelper))

    buildEntry(
      urlId,
      title,
      summary,
      content,
      date,
      contentHash,
      timeToNextCrawl,
      disabled,
      maybeConnectToAndCreateTags
    )
  }

  def connectToOrCreateTag(
      tags: Seq[String],
      graphQHelper: GraphQLHelper
  ): (Seq[TagWhereUniqueInput], Seq[TagCreateInput]) = {
    val existingTags = graphQHelper.existingTags(tags)

    /* Build connections to existing entries */
    val tagToMatchingTag: Map[String, Option[TagWhereUniqueInput]] =
      mapTagToExistingTag(tags, existingTags)

    /* Build query to create new tags */
    val createTags = createModelToCreateTag(
      tagToMatchingTag.filter(_._2.isEmpty).keys.toSeq
    )

    (tagToMatchingTag.values.flatten.toSeq, createTags)
  }

  private def mapTagToExistingTag(
      tags: Seq[String],
      existingTags: Seq[TagView[String]]
  ): Map[String, Option[TagWhereUniqueInput]] =
    tags.map { tag =>
      tag -> existingTags
        .find { existingTag =>
          existingTag.name match {
            case Some(name) => name == tag
            case None       => false
          }
        }
        .map { matchedTag =>
          TagWhereUniqueInput(id = Some(matchedTag.id))
        }
    }.toMap

  private def createModelToCreateTag(
      tags: Seq[String]
  ): Seq[TagCreateInput] =
    tags.map { tag =>
      TagCreateInput(name = Some(tag), generated = Some(false))
    }

  private def buildEntry(
      urlId: String,
      title: String,
      summary: Option[String],
      content: Option[String],
      date: Option[String],
      contentHash: String,
      timeToNextCrawl: Duration,
      disabled: Boolean = false,
      maybeConnectToAndCreateTags: Option[
        (Seq[TagWhereUniqueInput], Seq[TagCreateInput])
      ]
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntry.SimpleEntryView[SimpleUrl.SimpleUrlView, TagView[String]]
  ]] =
    Mutation.createEntry(
      Some(
        EntryCreateInput(
          name = Some(title),
          content = content,
          summary = summary,
          date = date,
          url = Some(
            UrlRelateToOneInput(
              connect = Some(UrlWhereUniqueInput(id = Some(urlId)))
            )
          ),
          contentHash = Some(contentHash),
          disabled = Some(disabled),
          nextCrawl = determineNextCrawl(timeToNextCrawl),
          tags = buildTagRelationInput(maybeConnectToAndCreateTags)
        )
      )
    )(
      SimpleEntry
        .view(SimpleUrl.view, Tag.view(CoVerifiedClientSchema.Language.id))
    )

  private def determineNextCrawl(timeToNextCrawl: Duration): Option[String] = {
    val nextCrawlDateTime =
      ZonedDateTime.now(ZoneId.of("UTC")).plus(timeToNextCrawl)
    Some(
      "\\[UTC]$".r.replaceAllIn(
        DateTimeFormatter.ISO_DATE_TIME.format(nextCrawlDateTime),
        ""
      )
    )
  }

  private def buildTagRelationInput(
      maybeConnectToAndCreateTags: Option[
        (
            Seq[CoVerifiedClientSchema.TagWhereUniqueInput],
            Seq[CoVerifiedClientSchema.TagCreateInput]
        )
      ]
  ): Option[TagRelateToManyInput] = maybeConnectToAndCreateTags.flatMap {
    case (connectRelation, createRelation) =>
      val maybeConnectRelation = Option.when(connectRelation.nonEmpty)(
        connectRelation.map(Some(_)).toList
      )
      val maybeCreateRelation =
        Option.when(createRelation.nonEmpty)(createRelation.map(Some(_)).toList)

      (maybeCreateRelation, maybeConnectRelation) match {
        case (None, None) => None
        case (create, connect) =>
          Some(
            TagRelateToManyInput(
              create = create,
              connect = connect
            )
          )
      }
  }
}
