// These are settings shared among all of the projects.

val sharedSettings = Seq(
  scalaVersion := "3.3.1",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "0.7.29" % Test,
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
  )
)

lazy val day1 = project
  .in(file("day1"))
  .settings(sharedSettings)
