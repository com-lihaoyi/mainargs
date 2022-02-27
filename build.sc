import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import mill.scalalib.api.Util.isScala3
import scalalib._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima::0.0.4`
import com.github.lolgab.mill.mima._

val scala212 = "2.12.13"
val scala213 = "2.13.4"
val scala3 = "3.0.2"

val scalaVersions = Seq(scala212, scala213, scala3)
val scala2Versions = scalaVersions.filter(_.startsWith("2."))

val scalaJSVersions = for {
  scalaV <- scalaVersions
  scalaJSV <- Seq("1.4.0")
} yield (scalaV, scalaJSV)

val scalaNativeVersions = for {
  scalaV <- scala2Versions
  scalaNativeV <- Seq("0.4.0")
} yield (scalaV, scalaNativeV)

trait MainArgsPublishModule extends PublishModule with CrossScalaModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()
  def mimaPreviousVersions = Seq(
    VcsVersion
      .vcsState()
      .lastTag
      .getOrElse(throw new Exception("Missing last tag"))
  )
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

  def scalacOptions = super.scalacOptions() ++ (if (!isScala3(crossScalaVersion)) Seq("-P:acyclic:force") else Seq.empty)

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ (if (!isScala3(crossScalaVersion)) Agg(ivy"com.lihaoyi::acyclic:0.2.0") else Agg.empty)

  def compileIvyDeps = super.compileIvyDeps() ++ (if (!isScala3(crossScalaVersion)) Agg(
      ivy"com.lihaoyi::acyclic:0.2.0",
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
    ) else Agg.empty)

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-collection-compat::2.4.4"
  ) ++ Agg(ivy"com.lihaoyi::pprint:0.6.6")
}

trait Common extends CrossScalaModule {
  def millSourcePath = build.millSourcePath / "mainargs"
  def sources = T.sources(
    super.sources() ++ Seq(PathRef(millSourcePath / s"src-$platform"))
  )
  def platform: String
}

trait CommonTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.10")
  def sources = T.sources {
    val scalaMajor = if(isScala3(scalaVersion())) "3" else "2"
    super.sources() ++ Seq(
      millSourcePath / "src",
      millSourcePath / s"src-$platform",
      millSourcePath / s"src-$platform-$scalaMajor"
    ).map(PathRef(_))
  }
  def platform: String
}


object mainargs extends Module {
  object jvm extends Cross[JvmMainArgsModule](scalaVersions: _*)
  class JvmMainArgsModule(val crossScalaVersion: String)
    extends Common with ScalaModule with MainArgsPublishModule {
    def platform = "jvm"
    object test extends Tests with CommonTestModule{
      def platform = "jvm"
      def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::os-lib:0.7.8")
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
