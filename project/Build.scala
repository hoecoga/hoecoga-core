import sbt.Keys._
import sbt._

object Build extends sbt.Build {
  import Settings._

  lazy val root = Project(
    id = "hoecoga-core", base = file("."), settings = defaultSettings ++ jsonSettings ++ testSettings)
}

object Settings {
  val defaultSettings = Seq(
    name := "hoecoga-core",
    version := "1.0",
    scalaVersion := "2.11.6",
    scalacOptions in (Compile, compile) ++=
      Seq("-Xlint:-nullary-unit", "-Xlint", "-Xfatal-warnings", "-feature", "-unchecked", "-deprecation"))

  val jsonSettings = Seq(libraryDependencies += "com.typesafe.play" %% "play-json" % "2.3.9")

  val testSettings = Seq(
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2")
}
