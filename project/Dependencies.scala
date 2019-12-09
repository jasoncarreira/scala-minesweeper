import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "1.0.0-RC" withSources () withJavadoc ()
  lazy val pureApp = "com.github.battermann" %% "pureapp" % "0.6.0"
}
