name := "integrated-query-language"

organization := "net.andimiller"

isSnapshot := true

scalaVersion := "2.12.2"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.lihaoyi"   %% "fastparse"   % "0.4.3"
libraryDependencies += "org.typelevel" %% "cats"        % "0.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "0.4"

val circeVersion = "0.9.0-M1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2" % "test"
libraryDependencies += "org.scalatest"     %% "scalatest"       % "3.0.3" % "test"

bintrayReleaseOnPublish in ThisBuild := true

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
