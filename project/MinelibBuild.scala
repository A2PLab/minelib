import sbt._
import Keys._

object MinelibBuild extends Build {

  val dependencies = Seq(
    "org.scala-tools.sbinary" % "sbinary_2.9.0" % "0.4.0" withSources ())

  lazy val root = Project(id = "minelib",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      libraryDependencies := dependencies))
}

