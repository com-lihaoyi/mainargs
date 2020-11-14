import scala.language.experimental.macros
package object mainargs{
  def generateRoutes[T]: Seq[EntryPoint[T]] = macro RouterMacros.generateRoutesImpl[T]

}