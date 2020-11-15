import scala.language.experimental.macros
package object mainargs{
  implicit def generateEntryPoints[B]: EntryPoints[B] = macro Macros.generateEntryPoints[B]
  implicit def generateBareEntryPoints[B]: BareEntryPoints[B] = macro Macros.generateBareEntryPoints[B]

  implicit def genereateClassEntryPoints[T]: ClassEntryPoint[T] = macro Macros.genereateClassEntryPoints[T]

  type main = MainAnnotation
  type arg = ArgAnnotation
}
