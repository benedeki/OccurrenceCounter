name := "Occurrence Counter"
version := "1.0"
scalaVersion := "2.12.7"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

// for debugging sbt problems
logLevel := Level.Warn

scalacOptions += "-deprecation"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")