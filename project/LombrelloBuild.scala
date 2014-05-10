/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
import sbt._
import Keys._

object LombrelloBuildSettings {
  val sversion = "2.10.2"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ch.usi.inf.l3",
    version := "0.1-SNAPSHOT",
    mainClass in Compile := Some("ch.usi.inf.l3.lombrello.dsl.Main"),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"), // s"-doc-external-doc:${scalaInstance.value.libraryJar}#http://www.scala-lang.org/api/${scalaVersion.value}/",  "-sourcepath"),
    scalaVersion := sversion,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    licenses := ("BSD 3-Clause", new java.net.URL("http://opensource.org/licenses/BSD-3-Clause")) :: Nil,
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % sversion,
        "org.scala-lang" % "scala-compiler" % sversion),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)
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
