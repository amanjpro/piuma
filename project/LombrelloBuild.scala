import sbt._
import Keys._

object LombrelloBuildSettings {
  val sversion = "2.10.2"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    name := "lombrello",
    organization := "ch.usi.inf.l3",
    version := "0.1-SNAPSHOT",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
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
      run <<= run in Compile in tests
    )
   ) aggregate (main, tests)

  lazy val main: Project = Project(
    "main",
    file("src/main"),
    settings = buildSettings

  )

  lazy val tests: Project = Project(
    "tests",
    file("src/test"),
    settings = buildSettings ++ Seq(name := "tests")) dependsOn (main)
}