import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import scalalib._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima_mill0.9:0.0.4`
import com.github.lolgab.mill.mima._

val scala212 = "2.12.13"
val scala213 = "2.13.4"

val scalaJSVersions = for {
  scalaV <- Seq(scala213, scala212)
  scalaJSV <- Seq("1.4.0")
} yield (scalaV, scalaJSV)

val scalaNativeVersions = for {
  scalaV <- Seq(scala213, scala212)
  scalaNativeV <- Seq("0.4.0")
} yield (scalaV, scalaNativeV)

trait MainArgsPublishModule extends PublishModule with CrossScalaModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()
  def mimaPreviousVersions = VcsVersion.vcsState().lastTag.toSeq
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

  def scalacOptions = super.scalacOptions() ++ Seq("-P:acyclic:force")

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(ivy"com.lihaoyi::acyclic:0.2.0")

  def compileIvyDeps = super.compileIvyDeps() ++ Agg(
    ivy"com.lihaoyi::acyclic:0.2.0",
    ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
  )

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-collection-compat::2.4.0"
  )
}

trait Common extends CrossScalaModule {
  def millSourcePath = build.millSourcePath / "mainargs"
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platform"
  )
  def platform: String
}

trait CommonTestModule extends ScalaModule with TestModule {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.6")
  def testFrameworks = Seq("utest.runner.Framework")
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platform"
  )
  def platform: String
}


object mainargs extends Module {
  object jvm extends Cross[JvmMainArgsModule](scala212, scala213)
  class JvmMainArgsModule(val crossScalaVersion: String)
    extends Common with ScalaModule with MainArgsPublishModule {
    def platform = "jvm"
    object test extends Tests with CommonTestModule{
      def platform = "jvm"
      def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::os-lib:0.7.1")
    }
  }

  object js extends Cross[JSMainArgsModule](scalaJSVersions: _*)
  class JSMainArgsModule(val crossScalaVersion: String, crossJSVersion: String)
    extends Common with ScalaJSModule with MainArgsPublishModule {
    def platform = "js"
    def scalaJSVersion = crossJSVersion
    object test extends Tests with CommonTestModule{
      def platform = "js"
    }
  }

  object native extends Cross[NativeMainArgsModule](scalaNativeVersions: _*)
  class NativeMainArgsModule(val crossScalaVersion: String, crossScalaNativeVersion: String)
    extends Common with ScalaNativeModule with MainArgsPublishModule {
    def scalaNativeVersion = crossScalaNativeVersion
    def platform = "native"
    object test extends Tests with CommonTestModule{
      def platform = "native"
    }
  }
}

trait ExampleModule extends ScalaModule{
  def scalaVersion = "2.13.4"
  def moduleDeps = Seq(mainargs.jvm("2.13.4"))
}
object example{
  object hello extends ExampleModule
  object hello2 extends ExampleModule
  object caseclass extends ExampleModule
  object classarg extends ExampleModule
  object optseq extends ExampleModule

  object custom extends ExampleModule{
    def ivyDeps = Agg(ivy"com.lihaoyi::os-lib:0.7.1")
  }
  object vararg extends ExampleModule
  object vararg2 extends ExampleModule
}
