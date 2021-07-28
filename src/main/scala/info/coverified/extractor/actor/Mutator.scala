/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.actor

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import caliban.client.Operations.RootMutation
import caliban.client.SelectionBuilder
import info.coverified.extractor.analyzer.EntryInformation
import info.coverified.extractor.analyzer.EntryInformation.{
  CreateEntryInformation,
  UpdateEntryInformation
}
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
  UpdateEntry,
  UpdateUrl
}
import info.coverified.extractor.messages.SourceHandlerMessage.MutatorInitialized
import info.coverified.graphql.GraphQLHelper
import info.coverified.graphql.schema.CoVerifiedClientSchema.ArticleTag.ArticleTagView
import info.coverified.graphql.schema.CoVerifiedClientSchema.{
  ArticleTag,
  ArticleTagRelateToManyInput,
  ArticleTagWhereUniqueInput,
  EntryCreateInput,
  EntryUpdateInput,
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
import scala.concurrent.duration.FiniteDuration

/**
  * This actor takes care of facing all mutation to the GraphQL API
  */
object Mutator {
  def apply(): Behaviors.Receive[MutatorMessage] = uninitialized

  def uninitialized: Behaviors.Receive[MutatorMessage] =
    Behaviors.receive[MutatorMessage] {
      case (
          _,
          InitMutator(
            apiUri,
            authToken,
            reAnalysisInterval,
            distinctTagHandler,
            sourceHandler
          )
          ) =>
        val helper = new GraphQLHelper(apiUri, authToken)
        sourceHandler ! MutatorInitialized
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
            val updatedStateData = requestTagConsolidation(
              urlId,
              createEntryInformation,
              articleTags,
              stateData,
              context.self
            )
            idle(updatedStateData)
          case None =>
            context.log.debug(
              "Request to create an entry received. No need to harmonize tags. Create mutation."
            )
            createEntry(
              createEntryInformation,
              None,
              urlId,
              stateData.reAnalysisInterval,
              stateData.helper,
              context.log
            )
            Behaviors.same
        }

      case (context, UpdateEntry(updateEntryInformation, urlId, replyTo)) =>
        updateEntryInformation.tags match {
          case Some(articleTags) =>
            context.log.debug(
              "Request to update an entry received. Ask the distinct tag handler, to ensure a consistent state for this."
            )
            val updatedStateData = requestTagConsolidation(
              urlId,
              updateEntryInformation,
              articleTags,
              stateData,
              context.self
            )
            idle(updatedStateData)
          case None =>
            context.log.debug(
              "Request to update an entry received. No need to harmonize tags. Update content."
            )
            updateEntry(
              updateEntryInformation,
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
          case Some((entryInformation, urlId)) =>
            context.log.debug(
              "Actually create the mutation for entry '{}'.",
              contentHash
            )
            val connectTo = tagIds.map { tagId =>
              ArticleTagWhereUniqueInput(id = Some(tagId))
            }

            entryInformation match {
              case uei: UpdateEntryInformation =>
                updateEntry(
                  uei,
                  None,
                  urlId,
                  stateData.reAnalysisInterval,
                  stateData.helper,
                  context.log
                )
              case cei: CreateEntryInformation =>
                createEntry(
                  cei,
                  Some(connectTo),
                  urlId,
                  stateData.reAnalysisInterval,
                  stateData.helper,
                  context.log
                )
              case rei: EntryInformation.RawEntryInformation =>
                context.log.warn(
                  "Received unsupported entry information '{}'. Do not handle it.",
                  rei
                )
            }

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
      case (ctx, Terminate) if stateData.awaitTagConsolidation.nonEmpty =>
        ctx.log.debug(
          "Received termination request, but still awaiting {} harmonized tags. Wait another second.",
          stateData.awaitTagConsolidation.size
        )
        ctx.scheduleOnce(FiniteDuration.apply(1, "s"), ctx.self, Terminate)
        Behaviors.same
      case (ctx, Terminate) =>
        ctx.log.info("Shutting down mutator!")
        stateData.helper.close()
        Behaviors.stopped
      case _ => Behaviors.unhandled
    }

  /**
    * Ask the [[DistinctTagHandler]] to consolidate the given page tags
    *
    * @param urlId            Identifier of the url
    * @param entryInformation Entry information to keep in mind
    * @param articleTags      List of page provided tags
    * @param stateData        Current state of actor
    * @param self             Reference to self
    * @return Updated state data
    */
  private def requestTagConsolidation(
      urlId: String,
      entryInformation: EntryInformation,
      articleTags: List[String],
      stateData: MutatorStateData,
      self: ActorRef[MutatorMessage]
  ): MutatorStateData = {
    val contentHash = entryInformation.contentHash
    stateData.distinctTagHandler ! ConsolidateArticleTags(
      contentHash,
      articleTags,
      self
    )
    val updatedAwaitMap = stateData.awaitTagConsolidation + (contentHash -> (entryInformation, urlId))
    stateData.copy(awaitTagConsolidation = updatedAwaitMap)
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
  private def createEntry(
      createEntryInformation: CreateEntryInformation,
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]],
      urlId: String,
      reAnalysisInterval: Duration,
      graphQLHelper: GraphQLHelper,
      logger: Logger
  ): Unit = {
    /* Check, if there isn't yet an entry with the same content */
    val contentHash = createEntryInformation.contentHash.toString
    val disabled = graphQLHelper.existsEntryWithSameHash(contentHash)
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
          updatedAt = updatedNow,
          articleTags = buildTagRelationInput(maybeConnectToArticleTags)
        )
      )
    )(
      SimpleEntry
        .view(SimpleUrl.view, ArticleTag.view)
    )

  /**
    * Build a mutation to update the entry
    *
    * @param entryId                    Identifier of to be updated entry
    * @param urlId                      Identifier of url
    * @param title                      Title
    * @param maybeSummary               Optional summary information
    * @param maybeContent               Optional content information
    * @param maybeDate                  Optional date information
    * @param maybeConnectToArticleTags  An optional model to connect to article tags
    * @param disabled                   If the entry needs to be disabled
    * @param contentHash                The content hash information
    * @param timeToNextCrawl            Duration, until the next analysis shall wait
    * @return A selection builder forming the reply from API
    */
  private def updateEntry(
      entryId: String,
      urlId: String,
      title: String,
      maybeSummary: Option[String],
      maybeContent: Option[String],
      maybeDate: Option[String],
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]],
      disabled: Boolean,
      contentHash: String,
      timeToNextCrawl: Duration
  ): SelectionBuilder[RootMutation, Option[
    SimpleEntryView[SimpleUrlView, ArticleTagView]
  ]] =
    Mutation.updateEntry(
      id = entryId,
      data = Some(
        EntryUpdateInput(
          name = Some(title),
          hasBeenTagged = Some(false),
          url = Some(
            UrlRelateToOneInput(
              connect = Some(UrlWhereUniqueInput(id = Some(urlId)))
            )
          ),
          articleTags = buildTagRelationInput(maybeConnectToArticleTags),
          content = maybeContent,
          summary = maybeSummary,
          date = maybeDate,
          nextCrawl = determineNextCrawl(timeToNextCrawl),
          updatedAt = updatedNow,
          contentHash = Some(contentHash),
          disabled = Some(disabled)
        )
      )
    )(SimpleEntry.view(SimpleUrl.view, ArticleTag.view))

  private def determineNextCrawl(timeToNextCrawl: Duration): Option[String] = {
    val nextCrawlDateTime =
      ZonedDateTime.now(ZoneId.of("UTC")).plus(timeToNextCrawl)
    Some(toIsoDateTimeString(nextCrawlDateTime))
  }

  private def updatedNow: Option[String] =
    Some(toIsoDateTimeString(ZonedDateTime.now(ZoneId.of("UTC"))))

  private def toIsoDateTimeString(zdt: ZonedDateTime) =
    "\\[UTC]$".r.replaceAllIn(
      DateTimeFormatter.ISO_DATE_TIME.format(zdt),
      ""
    )

  /**
    * Finally builds the mutation and sends it to GraphQL-API
    *
    * @param updateEntryInformation     Raw information to create entry from
    * @param maybeConnectToArticleTags  Optional model to connect to several tags
    * @param urlId                      Id of the url, the article does belong to
    * @param reAnalysisInterval         Interval, when the next analysis shall be made
    * @param graphQLHelper              Helper to connect to GraphQL
    * @param logger                     Logging instance
    */
  private def updateEntry(
      updateEntryInformation: UpdateEntryInformation,
      maybeConnectToArticleTags: Option[Seq[ArticleTagWhereUniqueInput]],
      urlId: String,
      reAnalysisInterval: Duration,
      graphQLHelper: GraphQLHelper,
      logger: Logger
  ): Unit = {
    /* Check, if there isn't yet an entry with the same content */
    val contentHash = updateEntryInformation.contentHash.toString
    val disabled = graphQLHelper.existsEntryWithSameHash(
      contentHash,
      updateEntryInformation.id
    )
    if (disabled) {
      logger.warn(
        s"There is / are already entries available with the same content hash code. Update the entry, but disable it."
      )
    }

    val mutation = updateEntryInformation match {
      case UpdateEntryInformation(id, title, summary, content, date, _) =>
        updateEntry(
          id,
          urlId,
          title,
          summary,
          content,
          date,
          maybeConnectToArticleTags,
          disabled,
          contentHash,
          reAnalysisInterval
        )
    }

    graphQLHelper.saveEntry(mutation) match {
      case Some(_) =>
        logger.debug(
          "Entry '{}' successfully updated.",
          updateEntryInformation.id
        )
      case None =>
      /* TODO: Report to source handler */
    }
  }

  /**
    * Build a model to connect to related article tags
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
      awaitTagConsolidation: Map[Int, (EntryInformation, String)] = Map.empty
  )
}
