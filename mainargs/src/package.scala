import scala.language.experimental.macros
package object mainargs{
  implicit def generateRoutes[T]: EntryPoints[T] = macro Macros.generateRoutesImpl[T]
  def generateClassRoute[T, C]: EntryPoint[C] = macro Macros.generateClassRouteImpl[T, C]

  def parseArgsIntoClsOrExit[T](args: Array[String]): T = ???
  def parseArgsIntoClsOrThrow[T](args: Array[String]): T = ???
  def parseArgsIntoClsEither[T](args: Array[String]): Either[String, T] = ???

  def parseArgsRunMainOrExit[T: EntryPoints](args: Array[String]): Unit = {
    val eps = implicitly[EntryPoints[T]]
    eps
  }
  def parseArgsRunMainOrThrow[T: EntryPoints](args: Array[String]): Unit = ???
  def parseArgsRunMainEither[T: EntryPoints](args: Array[String]): Either[String, Unit] = ???


  import scala.annotation.{ClassfileAnnotation, StaticAnnotation}

  class arg(val name: String = null,
            val short: Char = 0,
            val doc: String = null,
            val flag: Boolean = false) extends ClassfileAnnotation

  class main(val name: String = null, val doc: String = null) extends ClassfileAnnotation
}