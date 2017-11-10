logLevel := Level.Warn

addSbtPlugin("me.lessis"         % "bintray-sbt" % "0.3.0")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.0")
addSbtPlugin("org.scala-js"     % "sbt-scalajs"              % "0.6.19")
addSbtPlugin("org.scala-native" % "sbt-crossproject"         % "0.2.2")  // (1)
addSbtPlugin("org.scala-native" % "sbt-scala-native"         % "0.3.3")  // (3)
addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.9.0")
