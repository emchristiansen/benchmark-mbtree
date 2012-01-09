import AssemblyKeys._

seq(assemblySettings: _*)



name := "benchmark_mbtree"

scalaVersion := "2.9.1"

scalacOptions ++= Seq(
"-optimize",
"-unchecked", 
"-deprecation"
)

//resolvers ++= Seq(
//"emchristiansen github" at "https://github.com/emchristiansen/mbtree"
//)

libraryDependencies ++= Seq(
"default" %% "mbtree" % "0.1-SNAPSHOT",
"org.scalatest" %% "scalatest" % "latest.integration",
"com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0",
"com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0"
)



