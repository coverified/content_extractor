/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder
import info.coverified.extractor.analyzer.EntryInformation.CreateEntryInformation
import info.coverified.extractor.messages.DistinctTagHandlerMessage.ConsolidateArticleTags
import info.coverified.extractor.messages.{
  DistinctTagHandlerMessage,
  MutatorMessage
}
import info.coverified.extractor.messages.MutatorMessage.{
  ConnectToTags,
  CreateEntry,
  InitMutator,
  Terminate,
  UpdateUrl
}
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  ArticleTagCreateInput,
  ArticleTagRelateToManyInput,
  ArticleTagWhereUniqueInput,
  EntryCreateInput,
  Mutation,
  UrlRelateToOneInput,
  UrlWhereUniqueInput
}
import info.coverified.graphql.schema.SimpleEntry.SimpleEntryView
import info.coverified.graphql.schema.{SimpleEntry, SimpleUrl}
import info.coverified.graphql.schema.SimpleUrl.SimpleUrlView
import org.slf4j.Logger

import java.time.{Duration, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

/**
  * This actor takes care of facing all mutation to the GraphQL API
  */
object Mutator {
  def apply(): Behaviors.Receive[MutatorMessage] = uninitialized

  def uninitialized: Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (
          _,
          InitMutator(apiUri, authToken, reAnalysisInterval, distinctTagHandler)
          ) =>
        val helper = new GraphQLHelper(apiUri, authToken)
        idle(MutatorStateData(helper, reAnalysisInterval, distinctTagHandler))
      case _ => Behaviors.unhandled
    }

  def idle(stateData: MutatorStateData): Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (context, CreateEntry(createEntryInformation, urlId, replyTo)) =>
        createEntryInformation.tags match {
          case Some(articleTags) =>
            context.log.debug(
              "Request to create an entry received. Ask the distinct tag handler, to ensure a consistent state for this."
            )
            val contentHash = createEntryInformation.contentHash
            stateData.distinctTagHandler ! ConsolidateArticleTags(
              contentHash,
              articleTags
            )
            val updatedAwaitMap = stateData.awaitTagConsolidation + (contentHash -> (createEntryInformation, urlId))
            idle(stateData.copy(awaitTagConsolidation = updatedAwaitMap))
          case None =>
            context.log.debug(
              "Request to create an entry received. No need to harmonize tags. Create mutation."
            )
            issueMutation(
              createEntryInformation,
              None,
              urlId,
              stateData.reAnalysisInterval,
              stateData.helper,
              context.log
            )
            Behaviors.same
        }

      case (context, ConnectToTags(contentHash, tagIds)) =>
        context.log.debug(
          "Received information, to which tags the content with hash code '{}' shall be connected.",
          contentHash
        )
        stateData.awaitTagConsolidation.get(contentHash) match {
          case Some((cei, urlId)) =>
            context.log.debug(
              "Actually create the mutation for entry '{}'.",
              contentHash
            )
            val connectTo = tagIds.map { tagId =>
              ArticleTagWhereUniqueInput(id = Some(tagId))
            }
            issueMutation(
              cei,
              Some(connectTo),
              urlId,
              stateData.reAnalysisInterval,
              stateData.helper,
              context.log
            )

            /* Update state information */
            val updatedAwaitingMap = stateData.awaitTagConsolidation.filterNot {
              case (key, _) => key == contentHash
            }
            idle(stateData.copy(awaitTagConsolidation = updatedAwaitingMap))
          case None =>
            context.log.warn(
              "I received tag connection information for content '{}', but I don't know that one.",
              contentHash
            )
            Behaviors.same
        }
      case (context, UpdateUrl(urlId, replyTo)) =>
        context.log.debug(
          "Attempting to update the url with id '{}'.",
          urlId
        )

        stateData.helper.updateUrl(urlId) match {
          case Some(id) =>
            context.log.debug(s"Updated url '$id'")
          case None =>
          // todo report to source handler
        }

        Behaviors.same
      case (ctx, Terminate) =>
        ctx.log.info("Shutting down mutator!")
        stateData.helper.close()
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  /**
    * Finally builds the mutation and sends it to GraphQL-API
    *
    * @param createEntryInformation     Raw information to create entry from
    * @param maybeConnectToArticleTags  Optional model to connect to several tags
    * @param urlId                      Id of the url, the article does belong to
    * @param reAnalysisInterval         Interval, when the next analysis shall be made
    * @param graphQLHelper              Helper to connect to GraphQL
    * @param logger                     Logging instance
    */
  private def issueMutation(
      createEntryInformation: CreateEntryInformation,
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]],
      urlId: String,
      reAnalysisInterval: Duration,
      graphQLHelper: GraphQLHelper,
      logger: Logger
  ): Unit = {
    /* Check, if there isn't yet an entry with the same content */
    val contentHash = createEntryInformation.contentHash.toString
    val disabled = !graphQLHelper.entriesWithSameHash(contentHash)
    if (disabled) {
      logger.warn(
        s"There is / are already entries available with the same content hash code. Create an entry, but disable it."
      )
    }

    val mutation = createEntryInformation match {
      case CreateEntryInformation(title, summary, content, date, _) =>
        buildEntry(
          urlId,
          title,
          summary,
          content,
          date,
          contentHash,
          reAnalysisInterval,
          disabled,
          maybeConnectToArticleTags
        )
    }

    graphQLHelper.saveEntry(mutation) match {
      case Some(_) =>
        logger.debug("Entry successfully stored.")
      case None =>
      /* TODO: Report to source handler */
    }
  }

  def connectToOrCreateTag(
      tags: Seq[String],
      graphQHelper: GraphQLHelper
  ): (Seq[ArticleTagWhereUniqueInput], Seq[ArticleTagCreateInput]) = {
    val existingTags = graphQHelper.existingTags(tags)

    /* Build connections to existing entries */
    val tagToMatchingTag: Map[String, Option[ArticleTagWhereUniqueInput]] =
      mapTagToExistingTag(tags, existingTags)

    /* Build query to create new tags */
    val createTags = createModelToCreateTag(
      tagToMatchingTag.filter(_._2.isEmpty).keys.toSeq
    )

    (tagToMatchingTag.values.flatten.toSeq, createTags)
  }

  private def mapTagToExistingTag(
      tags: Seq[String],
      existingTags: Seq[ArticleTagView]
  ): Map[String, Option[ArticleTagWhereUniqueInput]] =
    tags.map { tag =>
      tag -> existingTags
        .find { existingTag =>
          existingTag.name match {
            case Some(name) => name == tag
            case None       => false
          }
        }
        .map { matchedTag =>
          ArticleTagWhereUniqueInput(id = Some(matchedTag.id))
        }
    }.toMap

  private def createModelToCreateTag(
      tags: Seq[String]
  ): Seq[ArticleTagCreateInput] =
    tags.map { tag =>
      ArticleTagCreateInput(name = Some(tag))
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
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]]
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntryView[SimpleUrlView, ArticleTagView]
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
          articleTags = buildTagRelationInput(maybeConnectToArticleTags)
        )
      )
    )(
      SimpleEntry
        .view(SimpleUrl.view, ArticleTag.view)
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

  /**
    * Build a model to connect to related article tags
    * TODO: Check!!
    *
    * @param maybeConnectToArticleTags Optional list of models, that describe single tags to connect to
    * @return Optional model to describe necessary connections to article tags
    */
  private def buildTagRelationInput(
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]]
  ): Option[ArticleTagRelateToManyInput] =
    maybeConnectToArticleTags
      .flatMap { connectRelations =>
        Option.when(connectRelations.nonEmpty) {
          /* If there are models to point to single tags and the list ist not empty... */
          connectRelations.map(Some(_)).toList
        }
      }
      .map { connectToRelation =>
        /* Build an model to connect to all of these models */
        ArticleTagRelateToManyInput(
          connect = Some(connectToRelation)
        )
      }

  final case class MutatorStateData(
      helper: GraphQLHelper,
      reAnalysisInterval: Duration,
      distinctTagHandler: ActorRef[DistinctTagHandlerMessage],
      awaitTagConsolidation: Map[Long, (CreateEntryInformation, String)] =
        Map.empty
  )
}
