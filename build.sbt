organization := "io.underscore"

name := "simple-examples"

version := "0.0.1"

scalaVersion := "2.11.2"

scalaBinaryVersion := "2.11"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "releases" at "http://oss.sonatype.org/content/repositories/releases"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-language:_")

libraryDependencies ++= Seq (
  "org.scalaz"  %% "scalaz-effect" % "7.0.6"
)
