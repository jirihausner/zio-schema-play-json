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

addCommandAlias("mimaCheck", "+zioSchemaPlayJson/mimaReportBinaryIssues")

lazy val root = project
  .in(file("."))
  .settings(
    name                  := "zio-schema-play-json",
    publish / skip        := true,
    mimaPreviousArtifacts := Set.empty,
    testJVM               := {},
    testJS                := {},
  )
  .aggregate(
    zioSchemaPlayJsonJVM,
    zioSchemaPlayJsonJS,
    zioSchemaPlayJson210,
    zioSchemaPlayJson27,
    zioSchemaPlayJson26,
    zioSchemaPlayJsonJsoniterJVM,
    zioSchemaPlayJsonJsoniterJS,
    zioSchemaPlayJsonJsoniter210,
    zioSchemaPlayJsonJsoniter27,
    zioSchemaPlayJsonJsoniter26,
  )

lazy val zioSchemaPlayJson =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("zio-schema-play-json"))
    .enablePlugins(BuildInfoPlugin)
    .settings(stdSettings("zio-schema-play-json"))
    .settings(buildInfoSettings("zio.schema.codec.play.json"))
    .settings(mimaSettings(binCompatVersionToCompare, failOnProblem = true))
    .settings(
      Compile / unmanagedSources += {
        val file = (Compile / sourceManaged).value / "PlayJsonCompat.scala"
        IO.write(file, "package zio.schema.codec.play.json.internal\n\nprivate[play] trait PlayJsonCompat\n")
        file
      },
    )
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

lazy val zioSchemaPlayJsonJVM = zioSchemaPlayJson.jvm
  .settings(
    testJVM := (Test / test).value,
    testJS  := {}, // disable testing JS
  )

lazy val zioSchemaPlayJsonJS = zioSchemaPlayJson.js
  .settings(
    testJVM := {}, // disable testing JVM
    testJS  := (Test / test).value,
  )

lazy val zioSchemaPlayJson210 =
  project
    .in(file("zio-schema-play-json-210"))
    .settings(stdSettings("zio-schema-play-json-210"))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson210,
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)

lazy val zioSchemaPlayJson27 =
  project
    .in(file("zio-schema-play-json-27"))
    .settings(stdSettings("zio-schema-play-json-27", scalaVersions = Seq(Scala213, Scala212)))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson27,
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)

lazy val zioSchemaPlayJson26 =
  project
    .in(file("zio-schema-play-json-26"))
    .settings(stdSettings("zio-schema-play-json-26", scalaVersions = Seq(Scala212)))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-27" / "src" / "main" / "scala-2.12",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson26,
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)

lazy val zioSchemaPlayJsonJsoniter =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("zio-schema-play-json-jsoniter"))
    .enablePlugins(BuildInfoPlugin)
    .settings(stdSettings("zio-schema-play-json-jsoniter"))
    .settings(buildInfoSettings("zio.schema.codec.play.json.jsoniter"))
    .settings(mimaSettings(binCompatVersionToCompare, failOnProblem = true))
    .settings(
      Compile / unmanagedSources += {
        val file = (Compile / sourceManaged).value / "PlayJsonCompat.scala"
        IO.write(file, "package zio.schema.codec.play.json.internal\n\nprivate[play] trait PlayJsonCompat\n")
        file
      },
    )
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      resolvers += MavenRepository("artifactory-evolution-public", "https://evolution.jfrog.io/artifactory/public"),
      libraryDependencies ++= Seq(
        "org.playframework" %%% "play-json"             % Versions.playJson,
        "com.evolution"     %%% "play-json-jsoniter"    % Versions.playJsonJsoniter excludeAll {
          ExclusionRule().withOrganization("org.playframework")
        },
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

lazy val zioSchemaPlayJsonJsoniterJVM = zioSchemaPlayJsonJsoniter.jvm
  .settings(
    testJVM := (Test / test).value,
    testJS  := {},// disable testing JS
  )

lazy val zioSchemaPlayJsonJsoniterJS = zioSchemaPlayJsonJsoniter.js
  .settings(
    testJVM := {}, // disable testing JVM
    testJS  := (Test / test).value,
  )

lazy val zioSchemaPlayJsonJsoniter210 =
  project
    .in(file("zio-schema-play-json-jsoniter-210"))
    .settings(stdSettings("zio-schema-play-json-jsoniter-210"))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-210" / "src" / "main" / "scala",
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      resolvers += MavenRepository("artifactory-evolution-public", "https://evolution.jfrog.io/artifactory/public"),
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson210,
        "com.evolution"    %%% "play-json-jsoniter"    % Versions.playJsonJsoniter excludeAll {
          ExclusionRule().withOrganization("org.playframework")
        },
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)

lazy val zioSchemaPlayJsonJsoniter27 =
  project
    .in(file("zio-schema-play-json-jsoniter-27"))
    .settings(stdSettings("zio-schema-play-json-jsoniter-27", scalaVersions = Seq(Scala213, Scala212)))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Compile / unmanagedSourceDirectories += {
        val currentScalaVersion = (ThisBuild / scalaVersion).value
        if (currentScalaVersion.startsWith("2.12"))
          (ThisBuild / baseDirectory).value / "zio-schema-play-json-27" / "src" / "main" / "scala-2.12"
        else (ThisBuild / baseDirectory).value / "zio-schema-play-json-27" / "src" / "main" / "scala-2.13"
      },
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      resolvers += MavenRepository("artifactory-evolution-public", "https://evolution.jfrog.io/artifactory/public"),
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson27,
        "com.evolution"    %%% "play-json-jsoniter"    % Versions.playJsonJsoniter excludeAll {
          ExclusionRule().withOrganization("org.playframework")
        },
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)

lazy val zioSchemaPlayJsonJsoniter26 =
  project
    .in(file("zio-schema-play-json-jsoniter-26"))
    .settings(stdSettings("zio-schema-play-json-jsoniter-26", scalaVersions = Seq(Scala212)))
    .settings(
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "main" / "scala",
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-27" / "src" / "main" / "scala-2.12",
      Compile / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "main" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json" / "shared" / "src" / "test" / "scala",
      Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "zio-schema-play-json-jsoniter" / "shared" / "src" / "test" / "scala",
    )
    .settings(
      resolvers += MavenRepository("artifactory-evolution-public", "https://evolution.jfrog.io/artifactory/public"),
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json"             % Versions.playJson26,
        "com.evolution"    %%% "play-json-jsoniter"    % Versions.playJsonJsoniter excludeAll {
          ExclusionRule().withOrganization("org.playframework")
        },
        "dev.zio"           %% "zio"                   % Versions.zio,
        "dev.zio"           %% "zio-test"              % Versions.zio       % Test,
        "dev.zio"           %% "zio-test-sbt"          % Versions.zio       % Test,
        "dev.zio"           %% "zio-streams"           % Versions.zio,
        "dev.zio"           %% "zio-schema"            % Versions.zioSchema,
        "dev.zio"           %% "zio-schema-derivation" % Versions.zioSchema % Test,
        "dev.zio"           %% "zio-schema-zio-test"   % Versions.zioSchema % Test,
      ),
    )
    .settings(macroDefinitionSettings)
    .settings(Test / fork := true)
