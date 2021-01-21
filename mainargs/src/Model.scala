package mainargs


sealed trait ArgSig[T, B]{
  def widen[V >: T] = this.asInstanceOf[ArgSig[V, B]]
}
object ArgSig{
  def createVararg[T, B](name0: String,
                         arg: mainargs.arg)
                        (implicit argParser: ArgReader.Leftover[T])= {
    val name = scala.Option(arg.name).getOrElse(name0)
    val docOpt = scala.Option(arg.doc)
    Leftover[T, B](name, docOpt, argParser.reader)
  }
  def create[T, B](name0: String,
                   arg: mainargs.arg,
                   defaultOpt: Option[B => T])
                  (implicit argParser: ArgReader[T]): ArgSig[T, B] = {
    val nameOpt = scala.Option(arg.name).orElse(if (name0.length == 1 || arg.noDefaultName) None else Some(name0))
    val shortOpt = arg.short match{
      case '\u0000' => if (name0.length != 1 || arg.noDefaultName) None else Some(name0(0));
      case c => Some(c)
    }
    val docOpt = scala.Option(arg.doc)
    argParser match{
      case ArgReader.Flag() => ArgSig.Flag[B](nameOpt, shortOpt, docOpt)
      case ArgReader.Class(parser) => Class(parser.mains)
      case ArgReader.Leftover(reader: TokensReader[T]) =>
        Leftover[T, B](scala.Option(arg.name).getOrElse(name0), docOpt, reader)
      case ArgReader.Simple(reader) =>
        Simple[T, B](nameOpt, shortOpt, docOpt, defaultOpt, reader, arg.positional)
    }
  }


  sealed trait Terminal[T, B] extends ArgSig[T, B]{
    def name: Option[String]
    def doc: Option[String]
  }


  sealed trait Named[T, B] extends Terminal[T, B]{
    def shortName: Option[Char]
  }

  /**
   * Models what is known by the router about a single argument: that it has
   * a [[name]], a human-readable [[typeString]] describing what the type is
   * (just for logging and reading, not a replacement for a `TypeTag`) and
   * possible a function that can compute its default value
   */
  case class Simple[T, B](name: Option[String],
                          shortName: Option[Char],
                          doc: Option[String],
                          default: Option[B => T],
                          reader: TokensReader[T],
                          positional: Boolean) extends ArgSig.Named[T, B]{
    def typeString = reader.shortName
  }

  case class Flag[B](name: Option[String],
                     shortName: Option[Char],
                     doc: Option[String]) extends ArgSig.Named[mainargs.Flag, B]

  def flatten[T, B](x: ArgSig[T, B]): Seq[Terminal[T, B]] = x match{
    case x: Terminal[T, B] => Seq(x)
    case x: Class[T, B] =>
      x.reader.main.argSigs.flatMap(x => flatten(x.asInstanceOf[Terminal[T, B]]))
  }

  case class Class[T, B](reader: ClassMains[T]) extends ArgSig[T, B]

  case class Leftover[T, B](name0: String,
                            doc: Option[String],
                            reader: TokensReader[T]) extends ArgSig.Terminal[T, B]{
    def name = Some(name0)
  }
}

sealed trait ArgReader[T]
object ArgReader{
  implicit def createSimple[T: TokensReader]: Simple[T] = Simple(implicitly[TokensReader[T]])
  case class Simple[T](x: TokensReader[T]) extends ArgReader[T]

  implicit def createClass[T: SubParser]: Class[T] = Class(implicitly[SubParser[T]])
  case class Class[T](x: SubParser[T]) extends ArgReader[T]

  implicit def createLeftover[T: TokensReader]: Leftover[T] = Leftover(implicitly[TokensReader[T]])
  case class Leftover[T](reader: TokensReader[T]) extends ArgReader[mainargs.Leftover[T]]

  implicit def createFlag: Flag = Flag()
  case class Flag() extends ArgReader[mainargs.Flag]
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
 * Realistically, you will probably spend most of your time calling [[Invoker.invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class MainData[T, B](name: String,
                          argSigs0: Seq[ArgSig[_, B]],
                          doc: Option[String],
                          invokeRaw: (B, Seq[Any]) => T){

  val argSigs: Seq[ArgSig.Terminal[_, B]] =
    argSigs0.iterator.flatMap[ArgSig.Terminal[_, B]](ArgSig.flatten(_)).toVector
  val leftoverArgSig: Seq[ArgSig.Leftover[_, _]] =
    argSigs.collect{case x: ArgSig.Leftover[_, B] => x}

}

object MainData{
  def create[T, B](methodName: String,
                   main: mainargs.main,
                   argSigs: Seq[ArgSig[Any, B]],
                   invokeRaw: (B, Seq[Any]) => T) = {
    MainData(
      Option(main.name).getOrElse(methodName),
      argSigs,
      Option(main.doc),
      invokeRaw
    )
  }
}