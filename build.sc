import mill._
import mill.scalalib.publish.{Developer, License, PomSettings, VersionControl}
import scalalib._

object mainargs extends Cross[MainArgsModule]("2.12.12", "2.13.1")
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
//  def scalacOptions = Seq("-P:acyclic:force")

  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-collection-compat:2.1.4",
  )
  def compileIvyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
    ivy"com.lihaoyi::acyclic:0.2.0"
  )
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.3",
      ivy"com.lihaoyi::os-lib:0.7.1"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
