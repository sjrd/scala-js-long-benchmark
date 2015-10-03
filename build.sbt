enablePlugins(ScalaJSPlugin)

name := "scalajs-long-benchmark"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

persistLauncher in Compile := true

persistLauncher in Test := false

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.2"
)

jsDependencies += ProvidedJS / "benchmark.js" commonJSName "Benchmark"

scalaJSStage in Global := FastOptStage

scalaJSSemantics ~= { _.optimized }
