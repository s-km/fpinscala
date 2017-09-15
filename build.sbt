resolvers ++= Seq(Resolver.sonatypeRepo("releases"))

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.7"

lazy val root = (project in file("."))
  .settings(
    name := "fpinscala",
    scalaVersion := "2.12.3"
  )

