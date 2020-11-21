package mainargs

/**
 * Models what is known by the router about a single argument: that it has
 * a [[name]], a human-readable [[typeString]] describing what the type is
 * (just for logging and reading, not a replacement for a `TypeTag`) and
 * possible a function that can compute its default value
 */
case class ArgSig[T, B](name: String,
                        shortName: Option[Char],
                        doc: Option[String],
                        default: Option[B => T],
                        varargs: Boolean,
                        flag: Boolean,
                        reader: ArgReader[T]) extends AnyArgSig[T, B]{
  def typeString = reader.shortName
}

sealed trait AnyArgSig[T, B]{
  def widen[V >: T] = this.asInstanceOf[AnyArgSig[V, B]]
}
object AnyArgSig{
  def create[T, B](name: String,
                   arg: mainargs.arg,
                   vararg: Boolean, defaultOpt: Option[B => T])
                  (implicit argParser: AnyArgParser[T]): AnyArgSig[T, B] = {
    argParser match{
      case AnyArgParser.Class(parser) => ClassArgSig(parser.mains)
      case AnyArgParser.Simple(parser) =>
        ArgSig[T, B](
          scala.Option(arg.name).getOrElse(name),
          arg.short match{ case '\u0000' => None; case c => Some(c)},
          scala.Option(arg.doc),
          defaultOpt,
          vararg,
          arg.flag,
          parser
        )

    }
  }
  def flatten[T, B](x: AnyArgSig[T, B]): Seq[ArgSig[T, B]] = x match{
    case x: ArgSig[T, B] => Seq(x)
    case x: ClassArgSig[T, B] => x.reader.main.argSigs.flatMap(x => flatten(x.asInstanceOf[ArgSig[T, B]]))
  }
}
case class ClassArgSig[T, B](reader: ClassMains[T]) extends AnyArgSig[T, B]

sealed trait AnyArgParser[T]
object AnyArgParser{
  implicit def createSimple[T: ArgReader]: Simple[T] = Simple(implicitly[ArgReader[T]])
  case class Simple[T](x: ArgReader[T]) extends AnyArgParser[T]

  implicit def createClass[T: SubParser]: Class[T] = Class(implicitly[SubParser[T]])
  case class Class[T](x: SubParser[T]) extends AnyArgParser[T]
}

trait SubParser[T]{
  def mains: ClassMains[T]
}


case class MethodMains[B](value: Seq[MainData[Any, B]], base: () => B)

case class ClassMains[T](main: MainData[T, Any], companion: () => Any)

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[MainUtils.invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class MainData[T, B](name: String,
                          argSigs0: Seq[AnyArgSig[Any, B]],
                          doc: Option[String],
                          invokeRaw: (B, Seq[Any]) => T){

  val argSigs = argSigs0.iterator.flatMap(AnyArgSig.flatten).toVector
  val varargs = argSigs.exists(_.varargs)
}

object MainData{
  def create[T, B](methodName: String,
                   main: mainargs.main,
                   argSigs: Seq[AnyArgSig[Any, B]],
                   invokeRaw: (B, Seq[Any]) => T) = {
    MainData(
      Option(main.name).getOrElse(methodName),
      argSigs,
      Option(main.doc),
      invokeRaw
    )
  }
}