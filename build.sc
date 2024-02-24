import mill._, scalalib._, scalajslib._, scalanativelib._, publish._
import mill.scalalib.api.ZincWorkerUtil.isScala3
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.github.lolgab::mill-mima::0.0.23`
import de.tobiasroeser.mill.vcs.version.VcsVersion

import com.github.lolgab.mill.mima._

val scala212 = "2.12.17"
val scala213 = "2.13.10"
val scala3 = "3.1.3"

val osLib = "0.9.3"
val acyclic = "0.3.11"

val scalaVersions = List(scala212, scala213, scala3)

trait MainArgsPublishModule
    extends PublishModule
    with CrossScalaModule
    with Mima
    with PlatformScalaModule {

  def publishVersion = VcsVersion.vcsState().format()

  override def mimaPreviousVersions = Seq("0.6.0")

  override def versionScheme: T[Option[VersionScheme]] = T(Some(VersionScheme.EarlySemVer))

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

  def scalacOptions =
    super.scalacOptions() ++
      Option.when(!isScala3(scalaVersion()))("-P:acyclic:force")

  def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++
      Option.when(!isScala3(scalaVersion()))(ivy"com.lihaoyi:::acyclic:${acyclic}")

  def compileIvyDeps =
    super.compileIvyDeps() ++
      Agg.when(!isScala3(crossScalaVersion))(
        ivy"com.lihaoyi:::acyclic:${acyclic}",
        ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
      )

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-collection-compat::2.8.1"
  )
}

def scalaMajor(scalaVersion: String) = if (isScala3(scalaVersion)) "3" else "2"

trait CommonTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")
}

object mainargs extends Module {
  object jvm extends Cross[JvmMainArgsModule](scalaVersions)
  trait JvmMainArgsModule extends MainArgsPublishModule {
    object test extends ScalaTests with CommonTestModule {
      def ivyDeps = super.ivyDeps() ++ Agg(ivy"com.lihaoyi::os-lib:${osLib}")
    }
  }

  object js extends Cross[JSMainArgsModule](scalaVersions)
  trait JSMainArgsModule extends MainArgsPublishModule with ScalaJSModule {
    def scalaJSVersion = "1.10.1"
    object test extends ScalaJSTests with CommonTestModule
  }

  object native extends Cross[NativeMainArgsModule](scalaVersions)
  trait NativeMainArgsModule extends MainArgsPublishModule with ScalaNativeModule {
    def scalaNativeVersion = "0.4.7"
    object test extends ScalaNativeTests with CommonTestModule
  }
}

trait ExampleModule extends ScalaModule {
  def scalaVersion = scala213
  def moduleDeps = Seq(mainargs.jvm(scala213))
}

object example extends Module {
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
  object short extends ExampleModule
}
