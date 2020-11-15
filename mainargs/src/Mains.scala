package mainargs
import acyclic.skipped
import scala.language.experimental.macros

case class BareMains[B](value: Seq[MainData[B]])
object BareMains{
  implicit def generate[B]: BareMains[B] = macro Macros.generateBareMains[B]

}

case class Mains[B](value: Seq[MainData[B]], base: () => B)
object Mains{
  implicit def generate[B]: Mains[B] = macro Macros.generateMains[B]
}

case class ClassMains[T](main: MainData[Any], companion: () => Any)
object ClassMains{
  implicit def generate[T]: ClassMains[T] = macro Macros.genereateClassMains[T]
}

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[MainUtils.invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class MainData[B](name: String,
                   argSigs: Seq[ArgSig[B]],
                   doc: Option[String],
                   varargs: Boolean,
                   invoke0: (B, Map[String, String], Seq[String]) => Result[Computed[Any]])
