import sbt._
import Keys._

object BuildSettings {
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt,
    scalacOptions ++= Seq("-unchecked", "-deprecation")
    //unmanagedJars in Compile <+= unmanagedBase in Compile map { _ ** "*.jar" } 
//    unmanagedJars in Compile <+= unmanagedBase{ _ / "" / "main" / "srltk"},
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
  val scalatoolsrepo = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
}

object Dependencies {
  val junit = "junit" % "junit" % "4.8" % "test"
  val scalatest = "org.scalatest" % "scalatest_2.9.1" % "1.6.1"
  val liftframework = "net.liftweb" % "lift-framework_2.9.1" % "2.4-M4"
  val liftjson = "net.liftweb" % "lift-json_2.9.1" % "2.4-M4"
  val liftjsonext = "net.liftweb" % "lift-json-ext_2.9.1" % "2.4-M4"
  val scalaio = "com.github.scala-incubator.io" % "scala-io-core_2.9.1" % "0.2.0"
  val proguard = "org.scala-tools.sbt" % "sbt-proguard-plugin" % "0.0.5"
}


//----------------------------------------------------------------------
object srltkBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq (
    junit,scalatest,liftframework,liftjson,liftjsonext,scalaio
  )

  lazy val common = Project (
    "common",
    file ("srltk-common"),
    settings = buildSettings ++ Seq (libraryDependencies ++= commonDeps)
  )

  lazy val srltk = Project (
    "srltk",
    file ("."),
    settings = buildSettings
  ) aggregate (common)
}
