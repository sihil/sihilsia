import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "sihilsia"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.xhtmlrenderer" % "core-renderer" % "R8",
      "net.sf.jtidy" % "jtidy" % "r938",
      "net.sf.opencsv" % "opencsv" % "2.0"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
    )

}
