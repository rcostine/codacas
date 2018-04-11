val Http4sVersion = "0.18.4"
val Specs2Version = "4.0.3"
val LogbackVersion = "1.2.3"

lazy val root = (project in file("."))
  .settings(
    organization := "org.costine",
    name := "codacas",
    version := "1.0.1-SNAPSHOT",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.specs2"     %% "specs2-core"          % Specs2Version % "test",
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,
      "com.typesafe.akka" %% "akka-actor" % "2.5.11",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test
    )
  )

scalacOptions ++= Seq("-Ypartial-unification")

