name := baseDirectory.value.getName
version := "1.0-SNAPSHOT"
scalaVersion := "2.11.2"

val scalazVersion = "7.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.scalanlp" %% "breeze-viz" % "0.13"

)

scalacOptions += "-feature"

initialCommands in console := "import cats._, data._, implicits._"
