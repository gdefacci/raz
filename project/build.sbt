sbtVersion in Global := "0.13.5"

scalaVersion in Global := "2.10.4" 

parallelExecution in Test := false

//fork in Test := true

resolvers += Resolver.sonatypeRepo("releases")