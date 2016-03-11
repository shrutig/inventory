name := "inventory"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.typesafe.akka" %% "akka-remote" % "2.4.2",
  "org.jboss.aesh"        % "aesh"              % "0.66",
  "org.jboss.aesh"        % "aesh-extensions"   % "0.66",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2")
    