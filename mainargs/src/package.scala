import scala.language.experimental.macros
package object mainargs{
  implicit def generateRoutes[T]: EntryPoints[T] = macro Macros.generateRoutesImpl[T]
  def generateClassRoute[T, C]: EntryPoint[C] = macro Macros.generateClassRouteImpl[T, C]
  implicit def generateClassRoute[T]: ClassEntryPoint[T] = macro Macros.generateClassImplicitImpl[T]

  type main = MainAnnotation
  type arg = ArgAnnotation
}
