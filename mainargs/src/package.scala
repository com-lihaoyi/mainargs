import scala.language.experimental.macros
package object mainargs{
  implicit def generateRoutes[B]: EntryPoints[B] = macro Macros.generateRoutesImpl[B]
  def generateClassRoute[T, B]: EntryPoint[B] = macro Macros.generateClassRouteImpl[T, B]
  implicit def generateClassRoute[T]: ClassEntryPoint[T] = macro Macros.generateClassImplicitImpl[T]

  type main = MainAnnotation
  type arg = ArgAnnotation
}
