import mill._
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}
import scalalib._

object mainargs extends Cross[MainArgsModule]("2.12.12", "2.13.3")
class MainArgsModule(val crossScalaVersion: String) extends CrossScalaModule with PublishModule {
  def publishVersion = "0.1.0"
  def artifactName = "mainargs"
  def pomSettings = PomSettings(
    description = "Main method argument parser for Scala",
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/mainargs",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("lihaoyi", "mainargs"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )
  def compileIvyDeps = Agg(ivy"org.scala-lang:scala-reflect:$crossScalaVersion")
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.3",
      ivy"com.lihaoyi::ujson::1.1.0"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
