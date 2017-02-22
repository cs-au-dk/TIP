name := "tip"
version := "2.0.0"

scalaVersion := "2.11.8"

trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

////// runtime dependencies ////
libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"
libraryDependencies += "com.regblanc" % "scala-smtlib_2.11" % "0.2.1"

////// test dependencies ///////
libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
