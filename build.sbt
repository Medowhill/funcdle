enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"
ThisBuild / scalacOptions += "-Xlint:unused"

ThisBuild / libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1"
