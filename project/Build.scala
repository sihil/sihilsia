import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "sihilsia"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.xhtmlrenderer" % "flying-saucer-pdf-itext5" % "9.0.1",
      "net.sf.jtidy" % "jtidy" % "r938",
      "net.sf.opencsv" % "opencsv" % "2.0",
      "org.scala-sbt" %% "process" % "0.11.3"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
    )

}
