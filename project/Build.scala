import sbt._
import sbt.Keys._

object Build extends Build {

  // Dependencies


  val depends =  
  Seq("junit" % "junit" % "4.8" % "test",
      "org.scalatest" % "scalatest_2.9.1" % "1.6.1",
      "net.liftweb" % "lift-framework_2.9.1" % "2.4-M4",
      "net.liftweb" % "lift-json_2.9.1" % "2.4-M4",
      "net.liftweb" % "lift-json-ext_2.9.1" % "2.4-M4",
      "com.github.scala-incubator.io" % "scala-io-core_2.9.1" % "0.2.0"
    )

  // Settings
  val commonSettings = Defaults.defaultSettings ++ Seq(
    name := "srltk",
    organization := "University of Michigan",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    scalaVersion := "2.9.1",
    traceLevel := 0,
    libraryDependencies ++= depends,
    resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
    shellPrompt := { "sbt (%s)> " format projectId(_) })
  
  // Projects
  lazy val root: Project = Project("root",
      file("."),
      settings = commonSettings)

  // Helpers
  def projectId(state: State) = extracted(state).currentProject.id
  def extracted(state: State) = Project extract state
}


import sbt._
import Keys._

object BuildSettings {
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )
}

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
      "%s:%s:%s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object Resolvers {
  val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish" 
  val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"

  val oracleResolvers = Seq (sunrepo, sunrepoGF, oraclerepo)
}

object Dependencies {
  val logbackVer = "0.9.16"
  val grizzlyVer = "1.9.19"

  val logbackcore    = "ch.qos.logback" % "logback-core"     % logbackVer
  val logbackclassic = "ch.qos.logback" % "logback-classic"  % logbackVer

  val jacksonjson = "org.codehaus.jackson" % "jackson-core-lgpl" % "1.7.2"
  
  val grizzlyframwork = "com.sun.grizzly" % "grizzly-framework" % grizzlyVer
  val grizzlyhttp     = "com.sun.grizzly" % "grizzly-http"      % grizzlyVer
  val grizzlyrcm      = "com.sun.grizzly" % "grizzly-rcm"       % grizzlyVer
  val grizzlyutils    = "com.sun.grizzly" % "grizzly-utils"     % grizzlyVer
  val grizzlyportunif = "com.sun.grizzly" % "grizzly-portunif"  % grizzlyVer

  val sleepycat	= "com.sleepycat" % "je" % "4.0.92"

  val apachenet	  = "commons-net"   % "commons-net"   % "2.0"
  val apachecodec = "commons-codec" % "commons-codec" % "1.4"

  val scalatest	= "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
}

object CDAP2Build extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  // Sub-project specific dependencies
  val commonDeps = Seq (
    logbackcore,
    logbackclassic,
    jacksonjson,
    scalatest
  )

  val serverDeps = Seq (
    grizzlyframwork,
    grizzlyhttp,
    grizzlyrcm,
    grizzlyutils,
    grizzlyportunif,
    sleepycat,
    scalatest
  )

  val pricingDeps = Seq (apachenet, apachecodec, scalatest)
  
  lazy val cdap2 = Project (
    "cdap2",
    file ("."),
    settings = buildSettings
  ) aggregate (common, server, compact, pricing, pricing_service)

  lazy val common = Project (
    "common",
    file ("cdap2-common"),
    settings = buildSettings ++ Seq (libraryDependencies ++= commonDeps)
  )
			     
  lazy val server = Project (
    "server",
    file ("cdap2-server"),
    settings = buildSettings ++ Seq (resolvers := oracleResolvers, 
                                     libraryDependencies ++= serverDeps)
  ) dependsOn (common)

  lazy val pricing = Project (
    "pricing",
    file ("cdap2-pricing"),
    settings = buildSettings ++ Seq (libraryDependencies ++= pricingDeps)
  ) dependsOn (common, compact, server)

  lazy val pricing_service = Project (
    "pricing-service",
    file ("cdap2-pricing-service"),
    settings = buildSettings
  ) dependsOn (pricing, server)

  lazy val compact = Project (
    "compact",
    file ("compact-hashmap"),
    settings = buildSettings
  )
}
