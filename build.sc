import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import mill.scalalib.api.ZincWorkerUtil.isScala3
import scalalib._
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.3.0`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima::0.0.19`
import com.github.lolgab.mill.mima._

val scala212 = "2.12.17"
val scala213 = "2.13.10"
val scala3 = "3.1.3"

val osLib = "0.9.1"
val acyclic = "0.3.6"

val scalaVersions = List(scala212, scala213, scala3)

val scalaJSVersions = scalaVersions.map((_, "1.10.1"))
val scalaNativeVersions = scalaVersions.map((_, "0.4.7"))

trait MainArgsPublishModule extends PublishModule with CrossScalaModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()
  override def mimaPreviousVersions =
    Seq("0.2.3").filterNot(_ =>
      scalaVersion().startsWith("3.") && this.isInstanceOf[ScalaNativeModule]
    ) ++
      Seq("0.3.0")

  override def mimaPreviousArtifacts: T[Agg[Dep]] = T {
    if (mimaPreviousVersions().isEmpty) Agg.empty[Dep] else super.mimaPreviousArtifacts()
  }

  override def versionScheme: T[Option[VersionScheme]] = T(Some(VersionScheme.EarlySemVer))

  override def artifactName = "mainargs"
  def pomSettings = PomSettings(
    description = "Main method argument parser for Scala",
    organization = "com.lihaoyi",
    url = "https://github.com/com-lihaoyi/mainargs",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("com-lihaoyi", "mainargs"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi", "https://github.com/lihaoyi")
    )
  )

  def scalacOptions = super.scalacOptions() ++ (if (!isScala3(crossScalaVersion))
                                                  Seq("-P:acyclic:force")
                                                else Seq.empty)

  def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++ (if (!isScala3(crossScalaVersion))
                                      Agg(ivy"com.lihaoyi:::acyclic:${acyclic}")
                                    else Agg.empty)

  def compileIvyDeps =
    super.compileIvyDeps() ++ (if (!isScala3(crossScalaVersion)) Agg(
                                 ivy"com.lihaoyi:::acyclic:${acyclic}",
                                 ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
                               )
                               else Agg.empty)

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-collection-compat::2.8.1"
  ) ++ Agg(ivy"com.lihaoyi::pprint:0.8.1")
}

def scalaMajor(scalaVersion: String) = if (isScala3(scalaVersion)) "3" else "2"

trait Common extends CrossScalaModule {
  def millSourcePath = build.millSourcePath / "mainargs"
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platform",
    millSourcePath / s"src-${scalaMajor(scalaVersion())}",
    millSourcePath / s"src-${platform}-${scalaMajor(scalaVersion())}"
  )
  def platform: String
}

trait CommonTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platform",
    millSourcePath / s"src-${scalaMajor(scalaVersion())}",
    millSourcePath / s"src-${platform}-${scalaMajor(scalaVersion())}"
  )
  def platform: String
}

object mainargs extends Module {
  object jvm extends Cross[JvmMainArgsModule](scalaVersions: _*)
  class JvmMainArgsModule(val crossScalaVersion: String)
      extends Common with ScalaModule with MainArgsPublishModule {
    def platform = "jvm"
    object test extends Tests with CommonTestModule {
      def platform = "jvm"
      def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::os-lib:${osLib}")
    }
  }

  object js extends Cross[JSMainArgsModule](scalaJSVersions: _*)
  class JSMainArgsModule(val crossScalaVersion: String, crossJSVersion: String)
      extends Common with MainArgsPublishModule with ScalaJSModule {
    def platform = "js"
    def scalaJSVersion = crossJSVersion
    object test extends Tests with CommonTestModule {
      def platform = "js"
    }
  }

  object native extends Cross[NativeMainArgsModule](scalaNativeVersions: _*)
  class NativeMainArgsModule(val crossScalaVersion: String, crossScalaNativeVersion: String)
      extends Common with MainArgsPublishModule with ScalaNativeModule {
    def scalaNativeVersion = crossScalaNativeVersion
    def platform = "native"
    object test extends Tests with CommonTestModule {
      def platform = "native"
    }
  }
}

trait ExampleModule extends ScalaModule {
  def scalaVersion = scala213
  def moduleDeps = Seq(mainargs.jvm(scala213))
}
object example {
  object hello extends ExampleModule
  object hello2 extends ExampleModule
  object caseclass extends ExampleModule
  object classarg extends ExampleModule
  object optseq extends ExampleModule

  object custom extends ExampleModule {
    def ivyDeps = Agg(ivy"com.lihaoyi::os-lib:${osLib}")
  }
  object vararg extends ExampleModule
  object vararg2 extends ExampleModule
}
