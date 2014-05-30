/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
import sbt._
import Keys._

object LombrelloBuildSettings {
  val paradiseVersion = "2.0.0"
  val sversion = "2.11.1"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ch.usi.inf.l3",
    version := "0.1-SNAPSHOT",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"), 
    scalaVersion := sversion,
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % sversion,
        "org.scala-lang" % "scala-compiler" % sversion),
    libraryDependencies ++= (
        if(sversion.startsWith("2.10"))
          List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
        )
}

object LombrelloBuild extends Build {
  import LombrelloBuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in main in simple in kara,
      name := "root"
    )
   ) aggregate (main)

  lazy val main: Project = Project(
    "lombrello",
    base = file("lombrello"),
    settings = buildSettings ++ Seq(name := "lombrello")
  )

  lazy val simple: Project = Project(
    "simple",
    base = file("simple"),
    settings = buildSettings ++ Seq(
      name := "simple"
    )
  ) dependsOn (main)
  
  lazy val kara: Project = Project(
    "kara",
    base = file("kara"),
    settings = buildSettings ++ Seq(
      name := "kara"
    )
  ) dependsOn (main)
}
