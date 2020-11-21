package mainargs
import scala.annotation.tailrec

/**
 * Models what is known by the router about a single argument: that it has
 * a [[name]], a human-readable [[typeString]] describing what the type is
 * (just for logging and reading, not a replacement for a `TypeTag`) and
 * possible a function that can compute its default value
 */
case class ArgSig[B](name: String,
                     shortName: Option[Char],
                     doc: Option[String],
                     default: Option[B => Any],
                     varargs: Boolean,
                     flag: Boolean,
                     reader: ArgParser[_]) extends AnyArgSig[B]{
  def typeString = reader.shortName
}

sealed trait AnyArgSig[B]
object AnyArgSig{
  def flatten[B](x: AnyArgSig[B]): Seq[ArgSig[B]] = x match{
    case x: ArgSig[B] => Seq(x)
    case x: ClassArgSig[_, B] => x.reader.mains.main.argSigs.flatMap(x => flatten(x.asInstanceOf[ArgSig[B]]))
  }
}
case class ClassArgSig[T, B](reader: ParserForClass[T]) extends AnyArgSig[B]

sealed trait AnyArgParser[T]
object AnyArgParser{
  implicit def createSimple[T: ArgParser]: Simple[T] = Simple(implicitly[ArgParser[T]])
  case class Simple[T](x: ArgParser[T]) extends AnyArgParser[T]

  implicit def createClass[T: ParserForClass]: Class[T] = Class(implicitly[ParserForClass[T]])
  case class Class[T](x: ParserForClass[T]) extends AnyArgParser[T]
}