import sbt._
import Keys._


object BuildSettings {
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion := buildScalaVersion,
    traceLevel := 0,
    testOptions in Test += Tests.Argument("-oD"),
    shellPrompt  := ShellPrompt.buildShellPrompt,
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    unmanagedJars in Compile <++= baseDirectory.map(bd => (bd / "lib" / "libs_amd64" ***) get)
  )
}

//    unmanagedSourceDirectories in Compile <+= baseDirectory{ _ / "src" / "main" / "srltk"},
//----------------------------------------------------------------------
// Shell prompt which show the current project, 
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
  )
  val buildShellPrompt = { 
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s:%s> ".format (
        currProject, currBranch
      )
    }
  }
}

//----------------------------------------------------------------------
object Resolvers {
  val nlprepo = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
  val scalatoolsrepo = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
}

object Dependencies {
  val junit = "junit" % "junit" % "4.8" % "test"
  val scalatest = "org.scalatest" % "scalatest_2.9.1" % "1.6.1"
  val scalaio = "com.github.scala-incubator.io" % "scala-io-core_2.9.1" % "0.2.0"
  val proguard = "org.scala-tools.sbt" % "sbt-proguard-plugin" % "0.0.5"
  val xmlgraphics = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.4"
  val jcommon = "jfree" % "jcommon" % "1.0.16"
  val jfree = "jfree" % "jfreechart" % "1.0.13"
}


//----------------------------------------------------------------------
object srltkBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq (
//    junit,scalatest,scalaio,proguard,xmlgraphics,netlib,arpack,jcommon,jfree
    junit,scalatest,scalaio,proguard,xmlgraphics,jcommon,jfree
  )

  lazy val srltk = Project (
    "srltk",
    file ("."),
    settings = buildSettings ++ Seq (resolvers := Seq(nlprepo,scalatoolsrepo),
                                     libraryDependencies ++= commonDeps)
  )
}
