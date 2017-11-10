name := "integrated-query-language"

organization := "net.andimiller"

isSnapshot := true

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.CrossProject

scalaVersion in ThisBuild := "2.11.11"

val circeVersion = "0.9.0-M1"
val sharedSettings = Seq(
  scalacOptions += "-Ypartial-unification",
  scalaVersion := "2.11.11",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases"
)

val jvmSettings = Seq(
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3",
  libraryDependencies += "org.typelevel" %% "cats" % "0.9.0",
  libraryDependencies += "org.typelevel" %% "cats-effect" % "0.4",
  libraryDependencies += "org.typelevel" %% "kittens" % "1.0.0-RC0",
  libraryDependencies += "io.circe" %% "circe-core" % "0.9.0-M1",
  libraryDependencies += "io.circe" %% "circe-generic" % "0.9.0-M1",
  libraryDependencies += "io.circe" %% "circe-parser" % "0.9.0-M1",
  libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2" % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.7" % "test",
  libraryDependencies += "ai.x" %% "diff" % "1.2.0" % "test"
)

val jsSettings = Seq(
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.3",
  libraryDependencies += "org.typelevel" %%% "cats" % "0.9.0",
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "0.4",
  libraryDependencies += "org.typelevel" %%% "kittens" % "1.0.0-RC0",
  libraryDependencies += "io.circe" %%% "circe-core" % "0.9.0-M1",
  libraryDependencies += "io.circe" %%% "circe-generic" % "0.9.0-M1",
  libraryDependencies += "io.circe" %%% "circe-parser" % "0.9.0-M1"
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
  .in(file("core"))
  .settings(sharedSettings)
  .jvmSettings(jvmSettings)
  .jsSettings(jsSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

bintrayReleaseOnPublish in ThisBuild := true

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
