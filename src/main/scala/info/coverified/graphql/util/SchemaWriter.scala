/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.graphql.util

import caliban.parsing.adt.Document
import caliban.tools.SchemaWriter._

/**
  * //ToDo: Class Description
  *
  * @version 0.1
  * @since 24.02.21
  */
object SchemaWriter {

  def write(
      schema: Document,
      objectName: Option[String] = None,
      packageName: Option[String] = None,
      effect: String = "zio.UIO"
  ): String = {
    val schemaDef = schema.schemaDefinition

    val argsTypes = schema.objectTypeDefinitions
      .flatMap(
        typeDef =>
          typeDef.fields.filter(_.args.nonEmpty).map(writeArguments(_, typeDef))
      )
      .mkString("\n")

    val unionTypes = schema.unionTypeDefinitions
      .map(
        union => (union, union.memberTypes.flatMap(schema.objectTypeDefinition))
      )
      .toMap

    val unions = unionTypes
      .map { case (union, objects) => writeUnion(union, objects) }
      .mkString("\n")

    val objects = schema.objectTypeDefinitions
      .filterNot(
        obj =>
          reservedType(obj) ||
            schemaDef.exists(_.query.getOrElse("Query") == obj.name) ||
            schemaDef.exists(_.mutation.getOrElse("Mutation") == obj.name) ||
            schemaDef
              .exists(_.subscription.getOrElse("Subscription") == obj.name) ||
            unionTypes.values.flatten.exists(_.name == obj.name)
      )
      .map(writeObject)
      .mkString("\n")

    val inputs =
      schema.inputObjectTypeDefinitions.map(writeInputObject).mkString("\n")

    val enums = schema.enumTypeDefinitions.map(writeEnum).mkString("\n")

    val queries = schema
      .objectTypeDefinition(schemaDef.flatMap(_.query).getOrElse("Query"))
      .map(t => writeRootQueryOrMutationDef(t, effect))
      .getOrElse("")

    val mutations = schema
      .objectTypeDefinition(schemaDef.flatMap(_.mutation).getOrElse("Mutation"))
      .map(t => writeRootQueryOrMutationDef(t, effect))
      .getOrElse("")

    val subscriptions = schema
      .objectTypeDefinition(
        schemaDef.flatMap(_.subscription).getOrElse("Subscription")
      )
      .map(t => writeRootSubscriptionDef(t))
      .getOrElse("")

    val hasSubscriptions = subscriptions.nonEmpty
    val hasTypes = argsTypes.length + objects.length + enums.length + unions.length + inputs.length > 0
    val hasOperations = queries.length + mutations.length + subscriptions.length > 0

    val typesAndOperations =
      s"""
      ${if (hasTypes)
        "object Types {\n" +
          argsTypes + "\n" +
          objects + "\n" +
          inputs + "\n" +
          unions + "\n" +
          enums + "\n" +
          "\n}\n"
      else ""}

      ${if (hasOperations)
        "object Operations {\n" +
          queries + "\n\n" +
          mutations + "\n\n" +
          subscriptions + "\n" +
          "\n}"
      else ""}
      """

    s"""${packageName.fold("")(p => s"package $p\n\n")}${if (hasTypes && hasOperations)
      s"import ${packageName.fold("")(p => s"$p")}.${objectName.fold("")(o => o)}.Types._\n"
    else ""}
          ${if (typesAndOperations.contains("@GQL"))
      "import caliban.schema.Annotations._"
    else ""}
          import info.coverified.graphql.schema.custom._\n
          ${objectName.fold("")(o => s"object $o {")}
          ${if (hasSubscriptions) "import zio.stream.ZStream" else ""}

      $typesAndOperations
       ${objectName.fold("")(_ => s"}")}
      """
  }

}
