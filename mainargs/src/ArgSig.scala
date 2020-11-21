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
                        flag: Boolean,
                        reader: ArgReader[T]) extends AnyArgSig.Terminal[T, B]{
  def typeString = reader.shortName
}

sealed trait AnyArgSig[T, B]{
  def widen[V >: T] = this.asInstanceOf[AnyArgSig[V, B]]
}
object AnyArgSig{
  sealed trait Terminal[T, B] extends AnyArgSig[T, B]{
    def name: String
    def doc: Option[String]
  }
  def create[T, B](name: String,
                   arg: mainargs.arg,
                   defaultOpt: Option[B => T])
                  (implicit argParser: AnyArgReader[T]): AnyArgSig[T, B] = {
    argParser match{
      case AnyArgReader.Class(parser) => ClassArgSig(parser.mains)
      case AnyArgReader.Leftover(reader: ArgReader[T]) =>
        LeftoverArgSig[T, B](Option(arg.name).getOrElse(name), Option(arg.doc), reader)
      case AnyArgReader.Simple(reader) =>
        ArgSig[T, B](
          scala.Option(arg.name).getOrElse(name),
          arg.short match{ case '\u0000' => None; case c => Some(c)},
          scala.Option(arg.doc),
          defaultOpt,
          arg.flag,
          reader
        )
    }
  }

  def flatten[T, B](x: AnyArgSig[T, B]): Seq[ArgSig[T, B]] = x match{
    case x: ArgSig[T, B] => Seq(x)
    case x: LeftoverArgSig[T, B] => Seq()
    case x: ClassArgSig[T, B] => x.reader.main.argSigs.flatMap(x => flatten(x.asInstanceOf[ArgSig[T, B]]))
  }
}
case class ClassArgSig[T, B](reader: ClassMains[T]) extends AnyArgSig[T, B]
case class LeftoverArgSig[T, B](name: String,
                                doc: Option[String],
                                reader: ArgReader[T]) extends AnyArgSig.Terminal[T, B]

sealed trait AnyArgReader[T]
object AnyArgReader{
  implicit def createSimple[T: ArgReader]: Simple[T] = Simple(implicitly[ArgReader[T]])
  case class Simple[T](x: ArgReader[T]) extends AnyArgReader[T]

  implicit def createClass[T: SubParser]: Class[T] = Class(implicitly[SubParser[T]])
  case class Class[T](x: SubParser[T]) extends AnyArgReader[T]

  implicit def createLeftover[T: ArgReader]: Leftover[T] = Leftover(implicitly[ArgReader[T]])
  case class Leftover[T](reader: ArgReader[T]) extends AnyArgReader[LeftoverTokens[T]]
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
  val leftoverArgSig: Seq[LeftoverArgSig[_, _]] = argSigs0
    .flatMap{
      case x: LeftoverArgSig[_, B] => Some(x)
      case x: ClassArgSig[_, B] => x.reader.main.leftoverArgSig
      case _ => None
    }
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