import scala.language.experimental.macros
import scala.annotation.ClassfileAnnotation
package object mainargs{
  implicit def generateRoutes[T]: EntryPoints[T] = macro Macros.generateRoutesImpl[T]
  def generateClassRoute[T, C]: EntryPoint[C] = macro Macros.generateClassRouteImpl[T, C]
  implicit def generateClassRoute[T]: ClassEntryPoint[T] = macro Macros.generateClassImplicitImpl[T]




  class arg(val name: String = null,
            val short: Char = 0,
            val doc: String = null,
            val flag: Boolean = false) extends ClassfileAnnotation

  class main(val name: String = null, val doc: String = null) extends ClassfileAnnotation
}
