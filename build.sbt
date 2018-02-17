
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
//resolvers += Resolver.mavenLocal

unmanagedBase := baseDirectory.value / "libs"

lazy val buildSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.huawei.scalan",
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
  scalacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-feature",
    "-Ywarn-adapted-args",
    "-Ywarn-inaccessible",
    "-Ywarn-nullary-override",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:experimental.macros"),
  publishTo := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  },
  // do not publish docs for snapshot versions
  publishArtifact in (Compile, packageDoc) := !version.value.trim.endsWith("SNAPSHOT"))

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test,
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    // TODO separate benchmark configuration, see https://github.com/scalameter/scalameter-examples/blob/master/basic-with-separate-config/build.sbt
    "com.storm-enroute" %% "scalameter" % "0.6" % Test),
  parallelExecution in Test := false,
  baseDirectory in Test := file("."),
  publishArtifact in Test := true,
  publishArtifact in (Test, packageSrc) := true,
  publishArtifact in (Test, packageDoc) := false,
  test in assembly := {})

lazy val commonSettings = buildSettings ++ testSettings

lazy val itSettings = commonSettings ++ Defaults.itSettings ++
  Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "it",
    javaOptions in IntegrationTest ++=
      Seq("-Xmx3g", "-XX:PermSize=384m", "-XX:MaxPermSize=384m", "-XX:ReservedCodeCacheSize=384m"),
    parallelExecution in IntegrationTest := false,
    fork in IntegrationTest := true)

def libraryDefSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq(
          s"-Xplugin:${file(".").absolutePath}/scalanizer/target/scala-2.11/scalanizer-assembly-0.3.0-SNAPSHOT.jar"
//          , s"-P:scalanizer:module=$scalanizerOption"
    //    , "-Xgenerate-phase-graph"
  )
)

lazy val allConfigDependency = "compile->compile;test->test"

cancelable in Global := true

lazy val common = Project("scalan-common", file("common"))
  .settings(commonSettings,
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "commons-io" % "commons-io" % "2.5"

//    "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.9.1"
  ))

lazy val meta = Project("scalan-meta", file("meta"))
  .dependsOn(common % allConfigDependency)
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "com.github.kxbmap" %% "configs-java7" % "0.3.0",
      "com.trueaccord.lenses" %% "lenses" % "0.4.12"
    ),
    fork in Test := true,
    fork in run := true)

val paradise = "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full

lazy val macros = Project("scalan-macros", file("macros"))
    .dependsOn(common % allConfigDependency, meta % allConfigDependency)
    .settings(commonSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "macro-compat" % "1.1.1"
      ))

lazy val scalanizer = Project("scalanizer", file("scalanizer"))
  .dependsOn(meta)
  .settings(commonSettings,
    publishArtifact in (Compile, packageBin) := false,
    assemblyOption in assembly ~= { _.copy(includeScala = false, includeDependency = true) },
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.copy(classifier = Some("assembly"))
    },
    addArtifact(artifact in (Compile, assembly), assembly)
  )

lazy val libraryapi = Project("library-api", file("library/library-api"))
  .dependsOn(meta, scalanizer, macros)
  .settings(
    commonSettings
//    libraryDefSettings
    :+ addCompilerPlugin(paradise),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "macro-compat" % "1.1.1"
    ))

lazy val libraryimpl = Project("library-impl", file("library/library-impl"))
  .dependsOn(meta, scalanizer, libraryapi % allConfigDependency)
  .settings(
    commonSettings,
    //libraryDefSettings,
    libraryDependencies ++= Seq())

lazy val library = Project("library", file("library/library"))
  .dependsOn(common % allConfigDependency, core % allConfigDependency, libraryimpl)
  .settings(commonSettings,
//    libraryDefSettings,
    libraryDependencies ++= Seq())

lazy val smartapi = Project("smart-api", file("smart/smart-api"))
    .dependsOn(common % allConfigDependency, meta, scalanizer, macros)
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        "org.scodec" %% "scodec-core" % "1.10.3",
        "org.typelevel" %% "macro-compat" % "1.1.1"
      ))

lazy val smartimpl = Project("smart-impl", file("smart/smart-impl"))
    .dependsOn(meta, scalanizer, smartapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq())

lazy val smart = Project("smart", file("smart/smart-library"))
    .dependsOn(common % allConfigDependency, core % allConfigDependency, smartimpl)
    .settings(//commonSettings,
      libraryDefSettings,
      libraryDependencies ++= Seq())

lazy val npuapi = Project("npu-api", file("npu/npu-api"))
    .dependsOn(meta, scalanizer, macros)
    .settings(libraryDefSettings :+ addCompilerPlugin(paradise),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "macro-compat" % "1.1.1"
//        , "ml.dmlc.mxnet" %% "mxnet-full_2.11-osx-x86_64-cpu" % "1.1.0-SNAPSHOT"
      ),
      unmanagedJars in Compile += file("libs/mxnet-core_2.11-1.1.0-SNAPSHOT.jar"))

lazy val npuimpl = Project("npu-impl", file("npu/npu-impl"))
    .dependsOn(meta, scalanizer,
      npuapi % allConfigDependency,
      libraryapi % allConfigDependency)
    .settings(libraryDefSettings,
      libraryDependencies ++= Seq(
        "org.scalanlp" %% "breeze" % "0.13.2"
        ),
      unmanagedJars in Compile += file("libs/mxnet-core_2.11-1.1.0-SNAPSHOT.jar"))

lazy val npu = Project("npu", file("npu/npu-library"))
    .dependsOn(
      common % allConfigDependency,
      core % allConfigDependency,
      libraryapi % allConfigDependency,
      libraryimpl % allConfigDependency,
      npuapi % allConfigDependency,
      npuimpl % allConfigDependency)
    .settings(//commonSettings,
      libraryDefSettings,
      libraryDependencies ++= Seq())

lazy val core = Project("scalan-core", file("core"))
  .dependsOn(common % allConfigDependency, meta % allConfigDependency, macros)
  .settings(commonSettings,
    libraryDependencies ++= Seq(
      "cglib" % "cglib" % "3.2.3",
      "org.objenesis" % "objenesis" % "2.4",
      "com.github.kxbmap" %% "configs-java7" % "0.3.0",
      "com.trueaccord.lenses" %% "lenses" % "0.4.12"
    ))

lazy val kotlinBackend = Project("scalan-kotlin-backend", file("kotlin-backend")).
  dependsOn(common % allConfigDependency, core % allConfigDependency, library)
  .configs(IntegrationTest)
  .settings(itSettings)

lazy val toolkit = Project("scalan-toolkit", file("toolkit")).
  dependsOn(common % allConfigDependency, meta % allConfigDependency, core % allConfigDependency, library % allConfigDependency)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq("io.spray" %%  "spray-json" % "1.3.3")
  )

lazy val root = Project("scalan", file("."))
  .aggregate(
    common, meta, macros, scalanizer, core,
    libraryapi, libraryimpl, library,
    smartapi, smartimpl, smart,
    npuapi, npuimpl, npu,
    kotlinBackend, toolkit)
  .settings(buildSettings, publishArtifact := false)

lazy val extraClassPathTask = TaskKey[String]("extraClassPath") // scalan.plugins.extraClassPath


