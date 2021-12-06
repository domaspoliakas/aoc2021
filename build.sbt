ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "poliakas.dev"
ThisBuild / run / fork := true

lazy val base = project
  .in(file("base"))
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % "3.2.0",
      "co.fs2" %% "fs2-io" % "3.2.0",
      "org.typelevel" %% "cats-effect" % "3.3.0",
      "org.typelevel" %% "cats-parse" % "0.3.6"
    )
  )

lazy val one = project.in(file("01")).dependsOn(base)
lazy val two = project.in(file("02")).dependsOn(base)
lazy val three = project.in(file("03")).dependsOn(base)
lazy val four = project.in(file("04")).dependsOn(base)
lazy val five = project.in(file("05")).dependsOn(base)
lazy val six = project.in(file("06")).dependsOn(base)
lazy val seven = project.in(file("07")).dependsOn(base)
lazy val eight = project.in(file("08")).dependsOn(base)
lazy val nine = project.in(file("09")).dependsOn(base)
lazy val ten = project.in(file("10")).dependsOn(base)
lazy val eleven = project.in(file("11")).dependsOn(base)
lazy val twelve = project.in(file("12")).dependsOn(base)
lazy val thirteen = project.in(file("13")).dependsOn(base)
lazy val fourteen = project.in(file("14")).dependsOn(base)
lazy val fifteen = project.in(file("15")).dependsOn(base)
lazy val sixteen = project.in(file("16")).dependsOn(base)
lazy val seventeen = project.in(file("17")).dependsOn(base)
lazy val eighteen = project.in(file("18")).dependsOn(base)
lazy val nineteen = project.in(file("19")).dependsOn(base)
lazy val twenty = project.in(file("20")).dependsOn(base)
lazy val twentyone = project.in(file("21")).dependsOn(base)
lazy val twentytwo = project.in(file("22")).dependsOn(base)
lazy val twentythree = project.in(file("23")).dependsOn(base)
lazy val twentyfour = project.in(file("24")).dependsOn(base)
