import sbt._

class TTTProject(info: ProjectInfo) extends DefaultProject(info) {

  val sc = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9"

  val st = "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

}