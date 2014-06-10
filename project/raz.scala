import sbt._
import Keys._

object RazBuild  extends Build {
  
  val buildOrganization = "org.obl"
  val buildVersion      = "0.5-SNAPSHOT"
  val buildScalaVersion = "2.10.4"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization 			:= buildOrganization,
    version      			:= buildVersion,
    scalaVersion 			:= buildScalaVersion
    //, crossScalaVersions := Seq("2.10.4", "2.11.0") // disabled since unfiltered-scalatest is not avaible for 2.11.0
  )

  val servletDep = "javax.servlet" % "servlet-api" % "2.5" 

  val unfilteredVersion = "0.7.1"
  val unfilteredCore = "net.databinder" %% "unfiltered" % unfilteredVersion
  val unfilteredFilter = "net.databinder" %% "unfiltered-filter" % unfilteredVersion
  
  val unfilteredTest = Seq(
    unfilteredFilter % "test",
    "net.databinder" %% "unfiltered-scalatest" % unfilteredVersion % "test",
    "net.databinder" %% "dispatch-http" % "0.8.9" % "test",
    servletDep % "test"
  )
  
  val testDeps = Seq(
	"junit" % "junit" % "4.10" % "test",
	"com.novocode" % "junit-interface" % "0.10" % "test"
  )
  
  lazy val raz = Project (
    "raz",
    file ("."),
    settings = buildSettings ++ Seq(
        libraryDependencies += unfilteredCore,
        libraryDependencies ++= testDeps,
        libraryDependencies ++= unfilteredTest
    )
  ) 

}
