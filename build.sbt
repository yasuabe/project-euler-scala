
val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "project-euler-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= List("-Ykind-projector:underscores"),

    libraryDependencies ++= List(
      "org.typelevel"     %% "cats-core"           % "2.6.1",
      "org.typelevel"     %% "algebra"             % "2.7.0",
      "org.typelevel"     %% "cats-laws"           % "2.7.0",
      "org.scalameta"     %% "munit"               % "0.7.29" % Test,
      "org.scalacheck"    %% "scalacheck"          % "1.16.0" % Test,
      "org.scalameta"     %% "munit-scalacheck"    % "0.7.29" % Test,
      "io.chrisdavenport" %% "cats-scalacheck"     % "0.3.1"  % Test,
      "org.typelevel"     %% "discipline-core"     % "1.5.0"  % Test,
      "org.typelevel"     %% "discipline-munit"    % "2.0.0-M2" % Test
    )
  )