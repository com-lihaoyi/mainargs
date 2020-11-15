import scala.language.experimental.macros
package object mainargs{
  implicit def generateMains[B]: Mains[B] = macro Macros.generateMains[B]
  implicit def generateBareMains[B]: BareMains[B] = macro Macros.generateBareMains[B]

  implicit def genereateClassMains[T]: ClassMains[T] = macro Macros.genereateClassMains[T]

  type main = MainAnnotation
  type arg = ArgAnnotation
}
