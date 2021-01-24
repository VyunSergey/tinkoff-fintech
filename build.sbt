
scalaVersion := "2.11.12"
val ScalaTestVersion        = "3.2.2"
val ScalaCheckVersion       = "1.14.1"
val ScalaTestPlusVersion    = "3.2.2.0"
val ScalaMeterVersion       = "0.19"
val ScalaMockVersion        = "5.1.0"

lazy val organizationSettings = Seq(
  organization := "io.github.vyunsergey",
  name := "tinkoff-fintech",
  homepage := Some(url("https://github.com/VyunSergey")),
  licenses := Seq(("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")))
)

lazy val testSettings = Seq(
  testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
  parallelExecution in Test := false,
  Test / testOptions := Seq(Tests.Filter(_.endsWith("Test"))),
  fork := true,
  outputStrategy := Some(StdoutOutput),
  connectInput := true
)

lazy val commonLibraryDependencies = Seq(
  // ScalaTest
  "org.scalatest"              %% "scalatest"          % ScalaTestVersion % Test,
  // ScalaCheck
  "org.scalacheck"             %% "scalacheck"         % ScalaCheckVersion % Test,
  // ScalaTestPlus
  "org.scalatestplus"          %% "scalacheck-1-14"    % ScalaTestPlusVersion % Test,
  // ScalaMeter
  "com.storm-enroute"          %% "scalameter"         % ScalaMeterVersion % Test,
  // ScalaMock
  "org.scalamock"              %% "scalamock"          % ScalaMockVersion % Test
)

lazy val scalaCompilerOptions = Seq(
  "-deprecation",                  // Emit warning and location for usages of deprecated APIs
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly
  "-encoding", "UTF-8",            // Specify character encoding used by source files
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:postfixOps",          // Allows operator syntax in postfix position (deprecated since Scala 2.10)
  "-Xfatal-warnings"               // Fail the compilation if there are any warnings
)

lazy val root = (project in file(".")).settings(
  organizationSettings,
  testSettings,
  libraryDependencies ++= commonLibraryDependencies,
  scalacOptions ++= scalaCompilerOptions
)
