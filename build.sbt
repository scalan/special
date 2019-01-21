import scala.util.Try

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

lazy val buildSettings = Seq(
  scalaVersion := "2.12.6",
  organization := "io.github.scalan",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xlint:-unused,_",
    "-feature",
    "-Ywarn-adapted-args",
    "-Ywarn-inaccessible",
    "-Ywarn-nullary-override",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:experimental.macros",
    "-opt:_"),
  publishTo := {
//    val nexus = "http://10.122.85.37:9081/nexus/"
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  },
  // do not publish docs for snapshot versions
  publishArtifact in(Compile, packageDoc) := !version.value.trim.endsWith("SNAPSHOT"))

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "org.scalacheck" %% "scalacheck" % "1.13.+" % Test,
    "com.storm-enroute" %% "scalameter" % "0.8.2" % Test,
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.10" % Test),
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
  publishArtifact in Test := true,
  publishArtifact in(Test, packageSrc) := true,
  publishArtifact in(Test, packageDoc) := false,
  test in assembly := {})

lazy val commonSettings = buildSettings ++ testSettings

lazy val itSettings = commonSettings ++ Defaults.itSettings ++
    Seq(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "it",
      javaOptions in IntegrationTest ++=
          Seq("-Xmx3g", "-Xss2m", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
      parallelExecution in IntegrationTest := false,
      fork in IntegrationTest := true)

def libraryDefSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq(
//    s"-Xplugin:${file(".").absolutePath }/scalanizer/target/scala-2.12/scalanizer-assembly-i8-more-ops-ec82dd81-SNAPSHOT.jar"
    //          , s"-P:scalanizer:module=$scalanizerOption"
    //    , "-Xgenerate-phase-graph"
  )
)

lazy val allConfigDependency = "compile->compile;test->test"

pomIncludeRepository := { _ => false }
cancelable in Global := true

lazy val common = Project("common", file("common"))
    .settings(commonSettings,
      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "commons-io" % "commons-io" % "2.5"

        //    "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.9.1"
      ))

lazy val meta = Project("meta", file("meta"))
    .dependsOn(common % allConfigDependency)
    .settings(commonSettings,
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "com.github.kxbmap" %% "configs" % "0.4.4",
        "com.trueaccord.lenses" %% "lenses" % "0.4.12"
      ),
      fork in Test := true,
      fork in run := true)

val paradise = "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full

lazy val macros = Project("macros", file("macros"))
    .dependsOn(common % allConfigDependency, meta % allConfigDependency)
    .settings(commonSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "macro-compat" % "1.1.1"
      ))

lazy val core = Project("core", file("core"))
    .dependsOn(common % allConfigDependency, meta % allConfigDependency, macros)
    .settings(commonSettings,
      libraryDependencies ++= Seq(
        "cglib" % "cglib" % "3.2.3",
        "org.objenesis" % "objenesis" % "2.4",
        "com.github.kxbmap" %% "configs" % "0.4.4",
        "com.trueaccord.lenses" %% "lenses" % "0.4.12"
      ))

lazy val plugin = Project("plugin", file("plugin"))
    .dependsOn(meta)
    .settings(commonSettings)


lazy val libraryapi = Project("library-api", file("library-api"))
    .dependsOn(meta, macros)
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "macro-compat" % "1.1.1"
      ))

lazy val libraryimpl = Project("library-impl", file("library-impl"))
    .dependsOn(meta, libraryapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(
        "org.spire-math" %% "debox" % "0.8.0",
        "org.scala-metal" %% "metal-core" % "0.16.0.1-SNAPSHOT",
        "org.scala-metal" %% "metal-library" % "0.16.0.1-SNAPSHOT"
      ))

lazy val library = Project("library", file("library"))
    .dependsOn(common % allConfigDependency, core % allConfigDependency, libraryimpl)
    .settings(//commonSettings,
      libraryDefSettings,
      libraryDependencies ++= Seq(
        "org.spire-math" %% "debox" % "0.8.0"
      ))

lazy val libraryconf = Project("library-conf", file("library-conf"))
    .dependsOn(plugin)
    .settings(commonSettings,
      libraryDependencies ++= Seq())

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
    .dependsOn(meta, plugin, libraryconf)
    .settings(commonSettings,
      publishArtifact := false,
//      publishArtifact in(Compile, packageBin) := false,
      assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
      artifact in(Compile, assembly) := {
        val art = (artifact in(Compile, assembly)).value
        art.withClassifier(Some("assembly"))
      },
      addArtifact(artifact in(Compile, assembly), assembly)
    )

lazy val kotlinBackend = Project("kotlin-backend", file("kotlin-backend")).
    dependsOn(common % allConfigDependency, core % allConfigDependency, library)
    .configs(IntegrationTest)
    .settings(itSettings)

lazy val toolkit = Project("toolkit", file("toolkit")).
    dependsOn(common % allConfigDependency, meta % allConfigDependency, core % allConfigDependency, library % allConfigDependency)
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq("io.spray" %% "spray-json" % "1.3.3")
    )

lazy val root = Project("special", file("."))
    .aggregate(common, meta, macros, core, plugin,
    libraryapi, libraryimpl, library, libraryconf, scalanizer, kotlinBackend, toolkit)
    .settings(buildSettings, publishArtifact := false)

lazy val extraClassPathTask = TaskKey[String]("extraClassPath") // scalan.plugins.extraClassPath

enablePlugins(GitVersioning)

version in ThisBuild := {
  if (git.gitCurrentTags.value.nonEmpty) {
    git.gitDescribedVersion.value.get
  } else {
    if (git.gitHeadCommit.value.contains(git.gitCurrentBranch.value)) {
      // see https://docs.travis-ci.com/user/environment-variables/#default-environment-variables
      if (Try(sys.env("TRAVIS")).getOrElse("false") == "true") {
        // pull request number, "false" if not a pull request
        if (Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false") {
          // build is triggered by a pull request
          val prBranchName = Try(sys.env("TRAVIS_PULL_REQUEST_BRANCH")).get
          val prHeadCommitSha = Try(sys.env("TRAVIS_PULL_REQUEST_SHA")).get
          prBranchName + "-" + prHeadCommitSha.take(8) + "-SNAPSHOT"
        } else {
          // build is triggered by a push
          val branchName = Try(sys.env("TRAVIS_BRANCH")).get
          branchName + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
        }
      } else {
        git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
      }
    } else {
      git.gitCurrentBranch.value + "-" + git.gitHeadCommit.value.get.take(8) + "-SNAPSHOT"
    }
  }
}

git.gitUncommittedChanges in ThisBuild := true

credentials += Credentials(Path.userHome / ".sbt" / ".special-sonatype-credentials")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

