name := "codacasNG"

version := "1.0"

lazy val `codacasng` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq( jdbc , anorm , cache , ws )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  
