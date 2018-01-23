lazy val common = Seq(
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test,
    "org.scalacheck" %% "scalacheck" % "1.13.5" % Test
  )
)

// Uncomment this and the tut dependency in plugin.sbt in order to build the tutorial.
enablePlugins(TutPlugin)

tutSourceDirectory := (sourceDirectory in curryhoward in Compile).value / "tut"
tutTargetDirectory := baseDirectory.value / "docs" //(crossTarget in core).value / "tut"
scalacOptions in Tut := scalacOptions.value.filterNot(disableWarningsForTut.contains)

lazy val disableWarningsForTut = Set("-Ywarn-unused", "-Xlint", "-Ywarn-unused-import")

lazy val errorsForWartRemover = Seq(Wart.EitherProjectionPartial, Wart.Enumeration, Wart.ExplicitImplicitTypes, Wart.FinalCaseClass, Wart.FinalVal, Wart.LeakingSealed, Wart.Return, Wart.StringPlusAny, Wart.TryPartial)

lazy val warningsForWartRemover = Seq(Wart.Equals, Wart.JavaConversions, Wart.IsInstanceOf, Wart.OptionPartial, Wart.TraversableOps) //Seq(Wart.Any, Wart.AsInstanceOf, Wart.ImplicitConversion, Wart.Option2Iterable, Wart.NoNeedForMonad, Wart.Nothing, Wart.Product, Wart.Serializable, Wart.ToString, Wart.While)

lazy val curryhoward: Project = (project in file("."))
  .settings(common)
  .settings(
    organization := "io.chymyst",
    version := "0.3.0",

    licenses := Seq("Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/Chymyst/curryhoward")),
    description := "Automatic code generation from function types using the Curry-Howard correspondence",

    //    scalacOptions += "-Ymacro-debug-lite",
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Ydelambdafy:inline",
      // "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args", // Makes calling a() fail to substitute a Unit argument into a.apply(x: Unit)
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ywarn-unused",
      "-Ywarn-unused-import", "-Xexperimental",
      "-target:jvm-1.8"
    ),
    scalacOptions ++= (
      if (scalaBinaryVersion.value != "2.11") Seq(
        "-target:jvm-1.8",
        "-opt:l:inline",
        "-Ypartial-unification",
        "-Yvirtpatmat"
      )
      else Nil
      ), // (SI-2712 pertains to partial-unification),

    wartremoverWarnings in(Compile, compile) ++= warningsForWartRemover,
    wartremoverErrors in(Compile, compile) ++= errorsForWartRemover,

    libraryDependencies ++= Seq(
      // We need scala-reflect because we use macros.
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )


/////////////////////////////////////////////////////////////////////////////////////////////////////
// Publishing to Sonatype Maven repository
publishMavenStyle := true
//
// pomIncludeRepository := { _ => false } // not sure we need this.
// http://www.scala-sbt.org/release/docs/Using-Sonatype.html says we might need it
// because "sometimes we have optional dependencies for special features".
//
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
//
publishArtifact in Test := false
//
/////////////////////////////////////////////////////////////////////////////////////////////////////
