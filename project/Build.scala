import sbt._
import Keys._

object BendARoniBuild extends Build {

  lazy val bendARoni = (project in file(".")
    aggregate(core, examples)
    dependsOn(core, examples)
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni-root"
    )
  )

  lazy val core = (project
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni"
    )
  )


  lazy val examples = (project
    dependsOn core
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni-examples",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2"
      )
    )
  )

  def commonSettings = 
    Seq(
      organization := "org.aylasoftware",
      scalaVersion := "2.11.7",
      scalacOptions := Seq(
          "-feature",
          "-language:higherKinds",
          "-Xfatal-warnings",
          "-deprecation",
          "-unchecked"
      ),

      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
      ),
 
      libraryDependencies ++= Seq(
        "com.chuusai" %% "shapeless" % "2.2.5" withSources(),
        "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
      )
    )
}
