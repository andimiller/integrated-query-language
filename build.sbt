name := "integrated-query-language"

version := "1.0"

scalaVersion := "2.11.7"


libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.4"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.7" % "test"
