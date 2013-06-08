name := "enstrophy"

organization := "gnos.us"

version := "0.1.0-SNAPSHOT"

description := "this project is a bag of stuff"

homepage := Some(url("https://github.com/troystribling/enstrophy"))

startYear := Some(2013)

licenses := Seq(
  ("MIT", url("http://opensource.org/licenses/MIT"))
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/troystribling/enstrophy"),
    "scm:git:https://github.com/troystribling/enstrophy.git",
    Some("scm:git:git@github.com:troystribling/enstrophy.git")
  )
)

organizationName := "us.gnos"

/* scala versions and options */
scalaVersion := "2.10.1"

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8"
  // "-Xcheckinit" // for debugging only, see https://github.com/paulp/scala-faq/wiki/Initialization-Order
  // "-optimise"   // this option will slow your build
)

scalacOptions ++= Seq(
  "-Yclosure-elim",
  "-Yinline"
)

// These language flags will be used only for 2.10.x.
// Uncomment those you need, or if you hate SIP-18, all of them.
scalacOptions <++= scalaVersion map { sv =>
  if (sv startsWith "2.10") List(
    "-Xverify",
    "-Ywarn-all",
    "-feature",
    "-language:reflectiveCalls",
    "-language:higherKinds",
    "-language:existentials",
    "-language:experimental.macros",
    "-language:experimental.dynamics"
  )
  else Nil
}

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

/* entry point */
mainClass in (Compile, packageBin) := Some("gnos.us.enstrophy.Main")

mainClass in (Compile, run) := Some("us.gnos.enstrophy.Main")

// CONTINUATIONS
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.1")

scalacOptions += "-P:continuations:enable"

/* dependencies */
libraryDependencies ++= Seq (
  // -- lang --
  // "org.apache.commons" % "commons-lang3" % "3.1",
  // "org.scalaz" %% "scalaz-core" % "7.0.0-M7",
  // "org.scalaz" %% "scalaz-effect" % "7.0.0-M7",
  // -- util --
  // "com.github.nscala-time" %% "nscala-time" % "0.2.0",
  // "org.spire-math" % "spire_2.10.0" % "0.3.0-M7",
  // "com.github.scopt" %% "scopt" % "2.1.0",
  // "org.rogach" %% "scallop" % "0.6.3",
  // -- collections --
  // "com.google.guava" % "guava" % "13.0.1",
  // "com.chuusai" %% "shapeless" % "1.2.3",
  // "de.sciss" %% "fingertree" % "1.2.+",
  // "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.0",
  // -- io --
  // "commons-io" % "commons-io" % "2.4",
  // -- logging & configuration --
  // "com.typesafe" %% "scalalogging-slf4j" % "1.0.0",
  // "ch.qos.logback" % "logback-classic" % "1.0.7" % "provided",
  // "com.typesafe" % "config" % "1.0.0",
  // -- database drivers --
  // "com.h2database" % "h2" % "1.2.127",
  // "mysql" % "mysql-connector-java" % "5.1.10",
  // -- persistence --
  // "com.novus" %% "salat" % "1.9.2-SNAPSHOT",
  // "net.debasishg" %% "redisclient" % "2.9",
  // "com.typesafe" %% "slick" % "1.0.0-RC1",
  // "org.squeryl" %% "squeryl" % "0.9.5-6",
  // "com.github.nikita-volkov" % "sorm" % "0.3.5",
  // "fi.reaktor" %% "sqltyped" % "0.1.0",
  // "com.imageworks.scala-migrations" %% "scala-migrations" % "1.1.1",
  // -- serialization --
  // "org.json4s" %% "json4s-native" % "3.1.0",
  // "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.1.3",
  // -- concurrency --
  // "com.typesafe.akka" %% "akka-actor" % "2.2-SNAPSHOT",
  // "org.scala-stm" %% "scala-stm" % "0.7",
  // -- network --
  // "net.databinder.dispatch" %% "dispatch-core" % "0.9.2",
  // -- testing --
  // "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  // "org.specs2" %% "specs2" % "1.13",
  "org.scalatest" % "scalatest_2.10" % "2.0.M5b",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"
)

/* you may need these repos */
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)

/* testing */
parallelExecution in Test := false

testOptions in Test += Tests.Argument("-oDF")

/* sbt behavior */
logLevel in compile := Level.Warn

traceLevel := 5

offline := false

/* publishing */
publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some(
    "snapshots" at nexus + "content/repositories/snapshots"
  )
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

mappings in (Compile, packageBin) ~= { (ms: Seq[(File, String)]) =>
  ms filter { case (file, toPath) =>
      toPath != "application.conf"
  }
}

initialCommands  in console := "import us.gnos.enstrophy.sort._;import us.gnos.enstrophy._"


publishArtifact in Test := false

// publishArtifact in (Compile, packageDoc) := false

// publishArtifact in (Compile, packageSrc) := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>troystribling</id>
      <name>Troy Stribling</name>
      <email>troy.stribling@gmail.com</email>
//    <url>http://johndoe.com</url>
    </developer>
  </developers>
)

// Josh Suereth's step-by-step guide to publishing on sonatype
// http://www.scala-sbt.org/using_sonatype.html

/* assembly plugin */
mainClass in AssemblyKeys.assembly := Some("gnos.us.enstrophy.Main")

assemblySettings

test in AssemblyKeys.assembly := {}

