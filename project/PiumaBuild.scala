/*
 * Copyright (c) <2013>, Amanj Sherwany <http://www.amanj.me>
 * All rights reserved.
 * */
import sbt._
import Keys._

object PiumaBuildSettings {
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
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object PiumaBuild extends Build {
  import PiumaBuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in main in simple in 
                kara in atomicScala in scalaDyno in
                mina in avro,
      run <<= run in Test in avro,
      scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits"),
      name := "root"
    )
   ) aggregate (main)

  lazy val main: Project = Project(
    "piuma",
    base = file("piuma"),
    settings = buildSettings ++ Seq(name := "piuma",
      // Create a virtual host on a server, and use it here
      publishTo := Some(Resolver.sftp("Piuma's Repo", 
                                      "euler.inf.unisi.ch",
                                      "public_html/repos/piuma")),
      publishMavenStyle := true
      // credentials, we load them from a file of the format:
      // realm=Piuma's Repo
      // host=piuma.amanj.me (or whatever)
      // user=admin
      // password=admin123
      // credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
    )
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

  lazy val atomicScala: Project = Project(
    "atomicScala",
    base = file("atomicScala"),
    settings = buildSettings ++ Seq(
      name := "atomicScala"
    )
  ) dependsOn (main)

  lazy val scalaDyno: Project = Project(
    "scalaDyno",
    base = file("scalaDyno"),
    settings = buildSettings ++ Seq(
      name := "scalaDyno"
    )
  ) dependsOn (main)

  lazy val mina: Project = Project(
    "mina",
    base = file("mina"),
    settings = buildSettings ++ Seq(
      name := "mina"
    )
  ) dependsOn (main)

  lazy val avro: Project = Project(
    "avro",
    base = file("avro"),
    settings = buildSettings ++ Seq(
      name := "avro",
      libraryDependencies ++= Seq("org.apache.avro" % "avro" % "1.7.5",
        "org.apache.avro" % "avro-compiler" % "1.7.5",
        "log4j" % "log4j" % "1.2.14")
        // "com.novocode" % "junit-interface" % "0.8",
        // "junit" % "junit" % "4.11")
    )
  ) dependsOn (main)


}
