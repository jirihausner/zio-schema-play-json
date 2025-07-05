import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.tools.mima.plugin.MimaKeys._
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import sbt._
import sbt.Keys._
import sbtbuildinfo._
import sbtbuildinfo.BuildInfoKeys._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtdynver.DynVerPlugin.autoImport.previousStableVersion
import scalafix.sbt.ScalafixPlugin.autoImport._

import java.util.{List => JList, Map => JMap}
import scala.jdk.CollectionConverters._

object BuildHelper {

  private val versions: Map[String, String] = {
    val doc  = new Load(LoadSettings.builder().build())
      .loadFromReader(scala.io.Source.fromFile(".github/workflows/ci.yml").bufferedReader())
    val yaml = doc.asInstanceOf[JMap[String, JMap[String, JMap[String, JMap[String, JMap[String, JList[String]]]]]]]
    val list = yaml.get("jobs").get("build").get("strategy").get("matrix").get("scala").asScala
    list.map(v => (v.split('.').take(2).mkString("."), v)).toMap
  }

  val Scala212: String = versions("2.12")
  val Scala213: String = versions("2.13")
  val Scala3: String   = versions("3.5")

  object Versions {

    val playJson              = "3.0.4"
    val playJson210           = "2.10.6"
    val playJson27            = "2.7.4"
    val playJson26            = "2.6.14"
    val jsoniter              = "2.33.2"
    val scalaJavaTime         = "2.6.0"
    val zio                   = "2.1.18"
    val zioSchema             = "1.7.0"
    val scalaCollectionCompat = "2.10.0"
  }

  def compilerOptions(scalaVersion: String, optimize: Boolean) = {
    val stdOptions = Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-language:existentials",
    ) ++ {
      if (sys.env.contains("CI")) {
        Seq("-Xfatal-warnings")
      } else {
        Seq()
      }
    }

    val std2xOptions = Seq(
      "-language:higherKinds",
      "-explaintypes",
      "-Yrangepos",
      "-Xlint:_,-missing-interpolator,-type-parameter-shadow,-infer-any",
      "-Ypatmat-exhaust-depth",
      "40",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Wconf:msg=lambda-parens:s",
      "-Xsource:3.0",
    )

    val optimizerOptions =
      if (optimize)
        Seq(
          "-opt:l:inline",
        )
      else Seq.empty

    val extraOptions = CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _))  =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros",
          "-Xkind-projector",
          "-source:3.0-migration",
          "-rewrite",
        )
      case Some((2, 13)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused",
          "-Ymacro-annotations",
          "-Ywarn-macros:after",
        ) ++ std2xOptions ++ optimizerOptions
      case Some((2, 12)) =>
        Seq(
          "-Ypartial-unification",
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Wconf:cat=unused-nowarn:s",
          "-Ywarn-unused-import",
        ) ++ std2xOptions ++ optimizerOptions
      case _             => Seq.empty
    }

    stdOptions ++ extraOptions
  }

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*): Seq[File] =
    for {
      platform <- List("shared", platform)
      version  <- "scala" :: versions.toList.map("scala-" + _)
      result = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVersion: String, platform: String, conf: String, baseDir: File): Seq[sbt.File] = {
    val versions = CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 11)) =>
        List("2.11", "2.11+", "2.11-2.12", "2.x")
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case _             =>
        List()
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "main",
        baseDirectory.value,
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "test",
        baseDirectory.value,
      )
    },
    Test / fork := crossProjectPlatform.value == JVMPlatform, // set fork to `true` on JVM to improve log readability, JS and Native need `false`
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == Scala3) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % Provided,
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided,
        )
    },
  )

  def buildInfoSettings(packageName: String) = Seq(
    buildInfoKeys    := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := packageName,
  )

  lazy val testJVM = taskKey[Unit]("Runs JVM tests for all applicable subprojects")
  lazy val testJS  = taskKey[Unit]("Runs JS tests for all applicable subprojects")

  def stdSettings(projectName: String, scalaVersions: Seq[String] = Seq(Scala213, Scala212, Scala3)) =
    Seq(
      name                          := projectName,
      crossScalaVersions            := scalaVersions,
      scalaVersion                  := scalaVersions.head,
      scalacOptions ++= compilerOptions(scalaVersion.value, optimize = !isSnapshot.value),
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) =>
            Seq(
              compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)),
              compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
            )
          case _            => List.empty
        }
      },
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, x)) if x <= 12 =>
            Seq(
              compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full)),
            )
          case _                       => List.empty
        }
      },
      ThisBuild / semanticdbEnabled := scalaVersion.value != Scala3,
      // ThisBuild / semanticdbOptions += "-P:semanticdb:synthetics:on",
      ThisBuild / semanticdbVersion := scalafixSemanticdb.revision,
      ThisBuild / scalafixDependencies ++= List(
        "com.github.vovapolu"                      %% "scaluzzi" % "0.1.23",
        "io.github.ghostbuster91.scalafix-unified" %% "unified"  % "0.0.9",
      ),
      Test / parallelExecution      := !sys.env.contains("CI"),
      incOptions ~= (_.withLogRecompileOnMacro(true)),
      autoAPIMappings               := true,
      testFrameworks                := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
      mimaPreviousArtifacts         := previousStableVersion.value.map(organization.value %% name.value % _).toSet,
      mimaCheckDirection            := "backward",
      mimaFailOnProblem             := true,
      testJVM                       := Def.taskDyn {
        val currentScalaVersion  = (ThisBuild / scalaVersion).value
        val projectScalaVersions = crossScalaVersions.value
        if (projectScalaVersions.contains(currentScalaVersion)) Test / test
        else {
          Keys.streams.value.log.warn(s"Skipping ${name.value}, Scala $currentScalaVersion is not supported!")
          Def.task {}
        }
      }.value,
      testJS                        := {},
    )

  def mimaSettings(binCompatVersionToCompare: Option[String], failOnProblem: Boolean): Seq[Def.Setting[?]] =
    binCompatVersionToCompare match {
      case None                   => Seq(mimaPreviousArtifacts := Set.empty)
      case Some(binCompatVersion) =>
        Seq(
          mimaPreviousArtifacts := Set(organization.value %% name.value % binCompatVersion),
          mimaFailOnProblem     := failOnProblem,
        )
    }
}
