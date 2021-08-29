lazy val common = Seq(
  scalaVersion := "2.13.6",
  crossScalaVersions := Seq("2.11.12", "2.12.13", "2.13.6"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  )
)
lazy val errorsForWartRemover = Seq(Wart.ArrayEquals, Wart.EitherProjectionPartial, Wart.Enumeration, Wart.Equals, Wart.ExplicitImplicitTypes, Wart.FinalCaseClass, Wart.FinalVal, Wart.LeakingSealed, Wart.Return, Wart.TryPartial)

lazy val warningsForWartRemover = Seq(Wart.Any, Wart.AnyVal, Wart.JavaConversions, Wart.IsInstanceOf, Wart.Option2Iterable, Wart.OptionPartial, Wart.StringPlusAny, Wart.TraversableOps) //Seq(Wart.AsInstanceOf, Wart.ImplicitConversion, Wart.NoNeedForMonad, Wart.Nothing, Wart.Product, Wart.Serializable, Wart.ToString, Wart.While)

// See http://tpolecat.github.io/2017/04/25/scalac-flags.html
lazy val scalacOptionsRobNorris = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding", "UTF-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  //  "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

lazy val scalacOptionsRobNorrisExtra212AndAbove = Seq(
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates" // Warn if a private member is unused.
)

lazy val scalacOptionsExtra212 = Seq(
  "-opt:l:inline",
  "-Ypartial-unification",
  "-Yvirtpatmat"
)

lazy val scalacOptionsExtra213 = scalacOptionsExtra212 ++ Seq(
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates" // Warn if a private member is unused.
)

lazy val myScalacOptions = Seq(
  "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ydelambdafy:inline",
  "-Xlint",
  "-Yno-adapted-args", // Makes calling a() fail to substitute a Unit argument into a.apply(x: Unit)
  "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Xexperimental",
  "-target:jvm-1.8"
)

lazy val scalacOptions212RemovedIn213 = Seq(
  "-Xlint:by-name-right-associative",
  "-Xlint:nullary-override",
  "-Xlint:unsound-match",
  "-Yno-adapted-args",
  "-Ywarn-unused-import",
  "-Ypartial-unification",
  "-Yvirtpatmat",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Xexperimental",
  "-Xfuture",
)

lazy val scalacOptionsBelow212 = (myScalacOptions ++ scalacOptionsRobNorris).distinct
lazy val scalacOptions212 = (myScalacOptions ++ scalacOptionsRobNorris ++ scalacOptionsRobNorrisExtra212AndAbove ++ scalacOptionsExtra212).distinct
lazy val scalacOptions213 = (myScalacOptions ++ scalacOptionsRobNorris ++ scalacOptionsRobNorrisExtra212AndAbove ++ scalacOptionsExtra213).distinct diff scalacOptions212RemovedIn213

lazy val curryhoward: Project = (project in file("."))
  .settings(common)
  .settings(
    organization := "io.chymyst",
    version := "0.3.9",

    licenses := Seq("Apache License, Version 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/Chymyst/curryhoward")),
    description := "Automatic code generation from function types using the Curry-Howard correspondence",

    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((x, y)) if x == 2 && y == 13 ⇒ scalacOptions213
        case Some((x, y)) if x == 2 && y == 12 ⇒ scalacOptions212
        case _ ⇒ scalacOptionsBelow212
      }
    }, // (SI-2712 pertains to partial-unification)

    wartremoverWarnings in(Compile, compile) ++= warningsForWartRemover,
    wartremoverErrors in(Compile, compile) ++= errorsForWartRemover,
    scalacOptions in(Compile, console) --= Seq("-Ywarn-unused:imports", "-Ywarn-unused-import", "-Ywarn-unused", "-Xfatal-warnings"),

    libraryDependencies ++= Seq(
      // We need scala-reflect because we use macros.
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      (if (scalaBinaryVersion.value startsWith "2.13")
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.15" % "1.3.0" % Test
      else
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
        ),
    )
  )


/////////////////////////////////////////////////////////////////////////////////////////////////////
// Publishing to Sonatype Maven repository
publishMavenStyle := true
publishTo := //sonatypePublishToBundle.value
{
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
