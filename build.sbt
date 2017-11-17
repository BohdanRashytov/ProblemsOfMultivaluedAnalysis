name := "ProblemsOfMultivaluedAnalysis"

version := "1.0"

scalaVersion := "2.12.3"
libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.2"

lazy val root = (project in file("."))
  .settings(name := "root")

lazy val pma1 = (project in file ("pma1"))
  .settings(
    libraryDependencies += "gov.nist.math" % "jama" % "1.0.3",
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathIO" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathArray" % "1.0"
  )
lazy val pma2 = (project in file ("pma2"))
  .settings(
    libraryDependencies += "gov.nist.math" % "jama" % "1.0.3",
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathIO" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathArray" % "1.0"
  )
lazy val pma3 = (project in file ("pma3"))
  .settings(
    libraryDependencies += "gov.nist.math" % "jama" % "1.0.3",
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathIO" % "1.0",
    libraryDependencies += "com.github.yannrichet" % "JMathArray" % "1.0"
  )
