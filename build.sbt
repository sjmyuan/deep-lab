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

resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)

// if your project uses both 2.10 and polymorphic lambdas
libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})

scalacOptions += "-feature"
scalacOptions += "-language:higherKinds"
scalacOptions += "-language:implicitConversions"

initialCommands in console := "import cats._, data._, implicits._"
