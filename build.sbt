name := "wumpusWorld"

version := "0.1"

scalaVersion := "2.12.12"

resolvers += Resolver.jcenterRepo

resolvers += Resolver.mavenCentral

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.15",
  "me.shadaj"  %% "scalapy-core" % "0.3.0"

)

fork := true
// use `pip show jep` to find your install location
javaOptions += "-Djava.library.path=/Users/larrysimon/anaconda3/lib/python3.6/site-packages/jep"
