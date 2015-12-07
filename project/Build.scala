import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object BendARoniBuild extends Build {

  lazy val bendARoni = (project in file(".")
    aggregate(coreJVM, coreJS, examples)
    dependsOn(coreJVM, coreJS, examples)
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni-root",
      publish := {},
      publishLocal := {}
    )
  )

  lazy val core = (crossProject.crossType(CrossType.Pure).in(file("core"))
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni",
      libraryDependencies ++= Seq(
        "org.aylasoftware" %%% "csparse" % "0.1-SNAPSHOT"
      )
    )
  )
  
  lazy val coreJVM = core.jvm
  lazy val coreJS = core.js


  lazy val examples = (project
    dependsOn coreJVM
    settings(commonSettings: _*)
    settings(
      moduleName := "bend-a-roni-examples",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2" withSources()
      ),
      publish := {},
      publishLocal := {}
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
