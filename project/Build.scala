import sbt._
import Keys._

object NinetyNineProblemsBuild extends Build {

  val root =
    Project(
      id = "ninety-nine-problems",
      base = file("."),
      settings =
          Seq(
            scalaVersion := "2.11.2",
            scalacOptions ++=
              Seq(
                "-unchecked",
                "-deprecation",
                "-feature",
                "-Xlint",
                "-language:reflectiveCalls"),
            testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Spec"))),
            resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
            libraryDependencies ++=
              Seq(
                "org.specs2" %% "specs2" % "2.4.6" % "test",
                "org.scalacheck" %% "scalacheck" % "1.11.6" % "test")))
}
