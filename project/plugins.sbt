addSbtPlugin("com.eed3si9n"       % "sbt-assembly"             % "2.3.1")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.14.3")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.5.5")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"          % "1.1.4")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.13.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.20.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.11.2")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.10"
