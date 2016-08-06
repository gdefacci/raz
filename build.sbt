organization  in ThisBuild  := "org.obl"
version       in ThisBuild  := "0.9.0-SNAPSHOT"
scalaVersion  in ThisBuild  := "2.11.8"

val buildSettings = Defaults.defaultSettings ++ Seq (
  scalaVersion 			:= buildScalaVersion
)
  
lazy val raz = Project (
    "raz",
    file("raz"),
    settings = buildSettings ++ Seq(
        libraryDependencies += scalazCore,
        libraryDependencies += shapeless,
        libraryDependencies += scalaCheck,
        libraryDependencies += scalaTest
    )
  )
  
lazy val razHttp4s = Project (
    "raz-http4s",
    file("raz-http4s"),
    settings = buildSettings ++ Seq(
        libraryDependencies += http4s
    )
  ).dependsOn(raz)  