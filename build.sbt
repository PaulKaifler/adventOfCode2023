// These are settings shared among all of the projects.

val sharedSettings = Seq(
  scalaVersion := "3.3.1",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.29" % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
  )
)

lazy val day01 = project
  .in(file("day01"))
  .settings(sharedSettings)

lazy val day02 = project
  .in(file("day02"))
  .settings(sharedSettings)

lazy val day03 = project
  .in(file("day03"))
  .settings(sharedSettings)