name := "vacareanu_project"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release"

libraryDependencies ++= {
  val luceneVersion = "7.7.1"
  val procVer = "8.1.3"
  Seq(
    "org.apache.lucene" % "lucene-core"             % luceneVersion,
    "org.apache.lucene" % "lucene-queryparser"      % luceneVersion,
    "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
    "org.scalatest"     %% "scalatest"              % "3.0.8" % Test,
    "com.typesafe"       % "config"                 % "1.4.1",
    "org.clulab"        %% "processors-main"        % procVer,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
}
