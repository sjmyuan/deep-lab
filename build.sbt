name := baseDirectory.value.getName
version := "1.0-SNAPSHOT"
scalaVersion := "2.11.2"

val scalazVersion = "7.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)

scalacOptions += "-feature"

initialCommands in console := "import cats._, data._, implicits._"
