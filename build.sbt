name := """cats-functional-data-validation"""

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

