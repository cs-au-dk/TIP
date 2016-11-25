scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"
libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2"
libraryDependencies +=   "junit" % "junit" % "4.12" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

scalaSource in Compile := baseDirectory.value / "src" 
scalaSource in Test := baseDirectory.value / "test"


