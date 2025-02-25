import BuildHelper._
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import xerial.sbt.Sonatype.sonatypeCentralHost

lazy val binCompatVersionToCompare = None

inThisBuild(
  List(
    organization := "io.github.jirihausner",
    homepage     := Some(url("https://github.com/jirihausner/zio-schema-play-json")),
    scmInfo      := Some(
      ScmInfo(
        url("https://github.com/jirihausner/zio-schema-play-json"),
        "git@github.com:jirihausner/zio-schema-play-json.git",
      ),
    ),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
      Developer(
        "jirihausner",
        "Jiri Hausner",
        "jiri.hausner.j@gmail.com",
        url("https://github.com/jirihausner"),
      ),
    ),
  ),
)

ThisBuild / sonatypeCredentialHost := sonatypeCentralHost
sonatypeRepository                 := "https://s01.oss.sonatype.org/service/local"

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll;fix")
addCommandAlias("fmtCheck", "all scalafmtSbtCheck scalafmtCheckAll")
addCommandAlias("fix", "scalafixAll")
addCommandAlias("fixCheck", "scalafixAll --check")

addCommandAlias("prepare", "fmt; fix")
addCommandAlias("lint", "fmtCheck; fixCheck")

addCommandAlias("testJVM", "zioSchemaPlayJsonJVM/test")
addCommandAlias("testJS", "zioSchemaPlayJsonJS/test")
addCommandAlias("testNative", "zioSchemaPlayJsonNative/test")

addCommandAlias("mimaCheck", "+zioSchemaPlayJson/mimaReportBinaryIssues")

lazy val root = project
  .in(file("."))
  .settings(
    name                  := "zio-schema-play-json",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty,
  )
  .aggregate(
    zioSchemaPlayJson.jvm,
    zioSchemaPlayJson.js,
    zioSchemaPlayJson.native,
  )

lazy val zioSchemaPlayJson =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("zio-schema-play-json"))
    .enablePlugins(BuildInfoPlugin)
    .settings(stdSettings("zio-schema-play-json"))
    .settings(buildInfoSettings("zio.schema.codec.play.json"))
    .settings(mimaSettings(binCompatVersionToCompare, failOnProblem = true))
    .settings(
      libraryDependencies ++= Seq(
        "org.playframework" %%% "play-json"             % Versions.playJson,
        "dev.zio"           %%% "zio"                   % Versions.zio,
        "dev.zio"           %%% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %%% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %%% "zio-streams"           % Versions.zio,
        "dev.zio"           %%% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %%% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %%% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(crossProjectSettings)
    .settings(Test / fork := crossProjectPlatform.value == JVMPlatform)
    .nativeSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time"      % Versions.scalaJavaTime,
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % Versions.scalaJavaTime,
      ),
    )
    .jsSettings(
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      scalaJSUseMainModuleInitializer := true,
    )
