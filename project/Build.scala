import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "sihilsia"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
    "pdf" % "pdf_2.9.1" % "0.2"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
      resolvers += Resolver.url("Violas Play Modules", url("http://www.joergviola.de/releases/"))(Resolver.ivyStylePatterns)
    )

}
