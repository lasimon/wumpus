name := "wumpusWorld"

version := "0.1"

scalaVersion := "2.12.12"

resolvers += Resolver.jcenterRepo

resolvers += Resolver.mavenCentral

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.15",
  "com.cra.figaro" %% "figaro" % "5.0.0.0"
)

//libraryDependencies ++= Seq(
//  "me.shadaj"  %% "scalapy-core" % "0.3.0+31-94930a4d"
//)

//fork := true
// use `pip show jep` to find your install location
//javaOptions += "-Djna.library.path=/usr/local/opt/python@3.7/Frameworks/Python.framework/Versions/3.7/lib"
