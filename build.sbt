ThisBuild / version := "1.0"
ThisBuild / organization := "org.example"

import com.typesafe.config._
val conf = ConfigFactory.parseFile(new File("version.conf")).resolve()
ThisBuild / scalaVersion := conf.getString("scalaVersion")
val spinalVersion = conf.getString("spinalVersion")

ThisBuild / scalacOptions ++= Seq("-deprecation")

val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val hw = (project in file("hw"))
  .settings(
    name := "hw",
    fork := true,
    Compile / scalaSource := baseDirectory.value / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
  )

lazy val all = (project in file("."))
  .settings(
    publishArtifact := false,
    publishLocal := {},
  )
  .aggregate(hw)

fork := true
