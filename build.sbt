import Dependencies._

ThisBuild / scalaVersion     := "2.12.10"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "jc"
ThisBuild / organizationName := "minesweeper"

lazy val root = (project in file("."))
  .settings(
    name := "minesweeper",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += catsEffect,
    libraryDependencies += pureApp,
  )

addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.1.0")

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")
