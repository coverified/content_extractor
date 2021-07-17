/**
 * © 2021. CoVerified,
 * Diehl, Fetzer, Hiry, Kilian, Mayer, Schlittenbauer, Schweikert, Vollnhals, Weise GbR
 **/

package info.coverified.extractor.profile

final case class ProfileConfig(
    profile: ProfileConfig.Profile
)
object ProfileConfig {
  final case class PageType(
      condition: ProfileConfig.PageType.Condition,
      examples: scala.List[java.lang.String],
      name: java.lang.String,
      selectors: ProfileConfig.PageType.Selectors
  )
  object PageType {
    final case class Condition(
        path: scala.Option[java.lang.String],
        selector: scala.Option[java.lang.String]
    )
    object Condition {
      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): ProfileConfig.PageType.Condition = {
        ProfileConfig.PageType.Condition(
          path =
            if (c.hasPathOrNull("path")) Some(c.getString("path")) else None,
          selector =
            if (c.hasPathOrNull("selector")) Some(c.getString("selector"))
            else None
        )
      }
    }

    final case class Selectors(
        audio: scala.Option[java.lang.String],
        breadcrumb: scala.Option[java.lang.String],
        content: ProfileConfig.PageType.Selectors.Content,
        date: scala.Option[java.lang.String],
        image: scala.Option[java.lang.String],
        subtitle: scala.Option[java.lang.String],
        summary: scala.Option[java.lang.String],
        tags: scala.Option[java.lang.String],
        title: java.lang.String,
        video: scala.Option[java.lang.String]
    )
    object Selectors {
      final case class Content(
          exclude_selector: scala.Option[scala.List[java.lang.String]],
          selector: java.lang.String
      )
      object Content {
        def apply(
            c: com.typesafe.config.Config,
            parentPath: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): ProfileConfig.PageType.Selectors.Content = {
          ProfileConfig.PageType.Selectors.Content(
            exclude_selector =
              if (c.hasPathOrNull("exclude_selector"))
                scala.Some(
                  $_L$_str(
                    c.getList("exclude_selector"),
                    parentPath,
                    $tsCfgValidator
                  )
                )
              else None,
            selector = $_reqStr(parentPath, c, "selector", $tsCfgValidator)
          )
        }
        private def $_reqStr(
            parentPath: java.lang.String,
            c: com.typesafe.config.Config,
            path: java.lang.String,
            $tsCfgValidator: $TsCfgValidator
        ): java.lang.String = {
          if (c == null) null
          else
            try c.getString(path)
            catch {
              case e: com.typesafe.config.ConfigException =>
                $tsCfgValidator.addBadPath(parentPath + path, e)
                null
            }
        }

      }

      def apply(
          c: com.typesafe.config.Config,
          parentPath: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): ProfileConfig.PageType.Selectors = {
        ProfileConfig.PageType.Selectors(
          audio =
            if (c.hasPathOrNull("audio")) Some(c.getString("audio")) else None,
          breadcrumb =
            if (c.hasPathOrNull("breadcrumb")) Some(c.getString("breadcrumb"))
            else None,
          content = ProfileConfig.PageType.Selectors.Content(
            if (c.hasPathOrNull("content")) c.getConfig("content")
            else com.typesafe.config.ConfigFactory.parseString("content{}"),
            parentPath + "content.",
            $tsCfgValidator
          ),
          date =
            if (c.hasPathOrNull("date")) Some(c.getString("date")) else None,
          image =
            if (c.hasPathOrNull("image")) Some(c.getString("image")) else None,
          subtitle =
            if (c.hasPathOrNull("subtitle")) Some(c.getString("subtitle"))
            else None,
          summary =
            if (c.hasPathOrNull("summary")) Some(c.getString("summary"))
            else None,
          tags =
            if (c.hasPathOrNull("tags")) Some(c.getString("tags")) else None,
          title = $_reqStr(parentPath, c, "title", $tsCfgValidator),
          video =
            if (c.hasPathOrNull("video")) Some(c.getString("video")) else None
        )
      }
      private def $_reqStr(
          parentPath: java.lang.String,
          c: com.typesafe.config.Config,
          path: java.lang.String,
          $tsCfgValidator: $TsCfgValidator
      ): java.lang.String = {
        if (c == null) null
        else
          try c.getString(path)
          catch {
            case e: com.typesafe.config.ConfigException =>
              $tsCfgValidator.addBadPath(parentPath + path, e)
              null
          }
      }

    }

    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): ProfileConfig.PageType = {
      ProfileConfig.PageType(
        condition = ProfileConfig.PageType.Condition(
          if (c.hasPathOrNull("condition")) c.getConfig("condition")
          else com.typesafe.config.ConfigFactory.parseString("condition{}"),
          parentPath + "condition.",
          $tsCfgValidator
        ),
        examples = $_L$_str(c.getList("examples"), parentPath, $tsCfgValidator),
        name = $_reqStr(parentPath, c, "name", $tsCfgValidator),
        selectors = ProfileConfig.PageType.Selectors(
          if (c.hasPathOrNull("selectors")) c.getConfig("selectors")
          else com.typesafe.config.ConfigFactory.parseString("selectors{}"),
          parentPath + "selectors.",
          $tsCfgValidator
        )
      )
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): java.lang.String = {
      if (c == null) null
      else
        try c.getString(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            null
        }
    }

  }

  final case class Profile(
      hostname: java.lang.String,
      pageTypes: scala.List[ProfileConfig.PageType]
  )
  object Profile {
    def apply(
        c: com.typesafe.config.Config,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): ProfileConfig.Profile = {
      ProfileConfig.Profile(
        hostname = $_reqStr(parentPath, c, "hostname", $tsCfgValidator),
        pageTypes = $_LProfileConfig_PageType(
          c.getList("pageTypes"),
          parentPath,
          $tsCfgValidator
        )
      )
    }
    private def $_LProfileConfig_PageType(
        cl: com.typesafe.config.ConfigList,
        parentPath: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): scala.List[ProfileConfig.PageType] = {
      import scala.jdk.CollectionConverters._
      cl.asScala
        .map(
          cv =>
            ProfileConfig.PageType(
              cv.asInstanceOf[com.typesafe.config.ConfigObject].toConfig,
              parentPath,
              $tsCfgValidator
            )
        )
        .toList
    }
    private def $_reqStr(
        parentPath: java.lang.String,
        c: com.typesafe.config.Config,
        path: java.lang.String,
        $tsCfgValidator: $TsCfgValidator
    ): java.lang.String = {
      if (c == null) null
      else
        try c.getString(path)
        catch {
          case e: com.typesafe.config.ConfigException =>
            $tsCfgValidator.addBadPath(parentPath + path, e)
            null
        }
    }

  }

  def apply(c: com.typesafe.config.Config): ProfileConfig = {
    val $tsCfgValidator: $TsCfgValidator = new $TsCfgValidator()
    val parentPath: java.lang.String = ""
    val $result = ProfileConfig(
      profile = ProfileConfig.Profile(
        if (c.hasPathOrNull("profile")) c.getConfig("profile")
        else com.typesafe.config.ConfigFactory.parseString("profile{}"),
        parentPath + "profile.",
        $tsCfgValidator
      )
    )
    $tsCfgValidator.validate()
    $result
  }

  private def $_L$_str(
      cl: com.typesafe.config.ConfigList,
      parentPath: java.lang.String,
      $tsCfgValidator: $TsCfgValidator
  ): scala.List[java.lang.String] = {
    import scala.jdk.CollectionConverters._
    cl.asScala.map(cv => $_str(cv)).toList
  }
  private def $_expE(
      cv: com.typesafe.config.ConfigValue,
      exp: java.lang.String
  ) = {
    val u: Any = cv.unwrapped
    new java.lang.RuntimeException(
      s"${cv.origin.lineNumber}: " +
        "expecting: " + exp + " got: " +
        (if (u.isInstanceOf[java.lang.String]) "\"" + u + "\"" else u)
    )
  }

  private def $_str(cv: com.typesafe.config.ConfigValue): java.lang.String = {
    java.lang.String.valueOf(cv.unwrapped())
  }

  private final class $TsCfgValidator {
    private val badPaths =
      scala.collection.mutable.ArrayBuffer[java.lang.String]()

    def addBadPath(
        path: java.lang.String,
        e: com.typesafe.config.ConfigException
    ): Unit = {
      badPaths += s"'$path': ${e.getClass.getName}(${e.getMessage})"
    }

    def addInvalidEnumValue(
        path: java.lang.String,
        value: java.lang.String,
        enumName: java.lang.String
    ): Unit = {
      badPaths += s"'$path': invalid value $value for enumeration $enumName"
    }

    def validate(): Unit = {
      if (badPaths.nonEmpty) {
        throw new com.typesafe.config.ConfigException(
          badPaths.mkString("Invalid configuration:\n    ", "\n    ", "")
        ) {}
      }
    }
  }
}
