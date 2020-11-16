package mainargs
import acyclic.skipped
import scala.language.experimental.macros

case class BasedMains[B](value: Seq[MainData[B]], base: () => B)

case class ClassMains[T](main: MainData[Any], companion: () => Any)

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
