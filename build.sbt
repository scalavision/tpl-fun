name := "tpl"

scalaVersion := "2.11.8"

version := "0.0.1-SNAPSHOT"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
//  "scalatest" %% "scalatest" % "latest.integration" % "test",
  "org.scalacheck" %% "scalacheck" % "latest.integration" % "test",
  "org.specs2" %% "specs2-core" % "latest.integration" % "test",
  "com.chuusai" %% "shapeless" % "2.3.2" % "test"
)

val root = 
  project.in(file("."))
