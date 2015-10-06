name := "ChessProblem"

version := "1.0"

scalaVersion := "2.11.7"

fork in test := true

scalacOptions ++= Seq(
  "-optimise",
  "-deprecation",
  "-encoding", "UTF-8",
  "-target:jvm-1.8",
  "-feature",
  "-unchecked",
  "-Xlint"
)

javaOptions in test += "-server -Xmx4G -XX:UseParallelGC -XX:UseParallelOldGC"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-async" % "0.9.5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test" // "1.12.2"
)
