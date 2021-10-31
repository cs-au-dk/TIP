name := "tip"

scalaVersion := "2.12.12"

trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.8"
libraryDependencies += "com.regblanc" % "scala-smtlib_2.12" % "0.2.1"

Compile / scalaSource := baseDirectory.value / "src"
