name := "wumpusWorld"

version := "0.1"

scalaVersion := "2.12.12"

resolvers += Resolver.jcenterRepo

resolvers += Resolver.mavenCentral

resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % "0.9.15"
)
