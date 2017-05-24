name := "integrated-query-language"

organization := "net.andimiller"

isSnapshot := true

scalaVersion := "2.12.2"

scalacOptions += "-Ypartial-unification"


libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.4"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.18.3"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.8.2" % "test"

bintrayReleaseOnPublish in ThisBuild := true

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
