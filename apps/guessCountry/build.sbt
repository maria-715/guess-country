lazy val guessCountry = (crossProject(JVMPlatform, JSPlatform) in file("."))
  .settings(name := "guesscountry", scalaVersion := "3.3.1")
  .jsSettings(test / aggregate := false, Test / test := {}, Test / testOnly := {})

lazy val client = project in file("./../../lib")
lazy val guessCountryJS = guessCountry.js.dependsOn(client)

lazy val server = project in file("./../../lib")
lazy val guessCountryJVM = guessCountry.jvm.dependsOn(server)
