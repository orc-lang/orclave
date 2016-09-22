name := "orcscal"
organization := "cs.utexas.edu"
version := "0.0.1"

scalaVersion in ThisBuild := "2.11.8"

run <<= run in Compile in orcscalTests

lazy val root = (project in file(".")).
  aggregate(orcscal, orcscalTests)

lazy val commonSettings = Seq(
  organization := "org.singingwizard",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-deprecation")
)

lazy val orcscal = (project in file("orcscal")).
  settings(commonSettings: _*).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val orcscalTests = (project in file("orcscal-tests")).
  settings(commonSettings: _*).settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    //scalacOptions ++= Seq("-Xprint:typer"),
    //scalacOptions ++= Seq("-uniqid"),
    //scalacOptions ++= Seq("-Xprint-types"),
    //scalacOptions ++= Seq("-Ylog:<phase>"),
    //scalacOptions ++= Seq("-Ylog:typer"),
    //scalacOptions ++= Seq("-Yshow-syms")
  ) dependsOn orcscal

