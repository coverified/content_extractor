package info.coverified.graphql.util

import caliban.parsing.adt.Document
import caliban.tools.{Formatter, IntrospectionClient}
import zio.{Task, UIO}

import java.io.{File, PrintWriter}

/**
 * //ToDo: Class Description
 *
 * @version 0.1
 * @since 24.02.21
 */
object SchemaUpdater {

  def main(args: Array[String]): Unit = {

    // params
    val url = "http://coverified-backend-keystone.docker/admin/api"
    val packageName = "info.coverified.graphql.schema"
    val objName = "CoVerifiedClientSchema"
    val outputPath = "src/main/scala/info/coverified/graphql/schema/CoVerifiedClientSchema.scala"

    // run code
    val runtime = zio.Runtime.default

    val introspectResult: Document = runtime.unsafeRun(IntrospectionClient.introspect(url,
      None))
    val generatedClientSchemaCode =
      ClientWriter.write(introspectResult, objName, packageName = Some(packageName), genView = true)

    val writeTask = for {
      formattedCode <- Formatter.format(generatedClientSchemaCode, None)
      _ <- Task(new PrintWriter(new File(outputPath)))
        .bracket(q => UIO(q.close()), pw => Task(pw.println(formattedCode)))
    } yield ()

    runtime.unsafeRun(writeTask)
  }
}
