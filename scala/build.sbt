val scalazCore = "org.scalaz" %% "scalaz-core" % "7.1.0"
val scalazConcurrent = "org.scalaz" %% "scalaz-concurrent" % "7.1.0"
val argonaut = "io.argonaut" %% "argonaut" % "6.1"

lazy val commonSettings = Seq(
  organization := "com.fpis",
  version := "0.1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fpis",
    libraryDependencies += scalazCore,
    libraryDependencies += scalazConcurrent,
    libraryDependencies += argonaut
  )
