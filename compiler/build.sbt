val scala3Version = "3.8.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "compiler",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.scala-lang" %% "toolkit" % "0.7.0"
   )
)
