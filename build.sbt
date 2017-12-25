val scalaV = "2.12.4"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.4" % Test
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.5" % Test

lazy val common = Seq(
  scalaVersion := scalaV,
  scalacOptions += "-deprecation",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
  libraryDependencies ++= Seq(
    scalatest, scalacheck
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
    licenses := Seq("Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/Chymyst/curryhoward")),
    description := "Automatic code generation from function types using the Curry-Howard correspondence",

    //    scalacOptions += "-Ymacro-debug-lite",
    scalacOptions ++= Seq(
      "-deprecation",
      "-unchecked",
      "-encoding", "UTF-8",
      "-feature",
      //    "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      //    "-opt:l:project", // this is deprecated
//      "-opt:l:inline",
//      "-Yvirtpatmat",
//      "-Ydelambdafy:inline",
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
      if (scalaBinaryVersion.value == "2.12") Seq("-target:jvm-1.8", "-Ypartial-unification")
      else Nil
      ), // (SI-2712 pertains to partial-unification),

    wartremoverWarnings in(Compile, compile) ++= warningsForWartRemover,
    wartremoverErrors in(Compile, compile) ++= errorsForWartRemover,

    libraryDependencies ++= Seq(
      // We only need the Scala compiler if we want to debug macros.
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided,
      // We need scala-reflect because we use macros.
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )
