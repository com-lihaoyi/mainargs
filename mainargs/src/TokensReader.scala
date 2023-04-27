package mainargs
import scala.collection.compat._
import scala.collection.mutable

/**
 * Represents the ability to parse CLI input arguments into a type [[T]]
 *
 * Has a fixed number of direct subtypes - [[Simple]], [[Flag]], [[Leftover]],
 * and [[Class]] - but each of those can be extended by an arbitrary number of
 * user-specified instances.
 */
sealed trait TokensReader[T] {
  def isLeftover: Boolean
  def isFlag: Boolean
}

object TokensReader {

  sealed trait Terminal[T] extends TokensReader[T]

  sealed trait ShortNamed[T] extends Terminal[T] {
    def shortName: String
  }

  trait Simple[T] extends ShortNamed[T] {
    def shortName: String
    def read(strs: Seq[String]): Either[String, T]
    def alwaysRepeatable: Boolean = false
    def allowEmpty: Boolean = false
    def isLeftover = false
    def isFlag = false
  }

  trait Flag extends Terminal[mainargs.Flag] {
    def isLeftover = false
    def isFlag = true
  }

  trait Leftover[T, V] extends ShortNamed[T] {
    def isLeftover = true
    def isFlag = false
    def read(strs: Seq[String]): Either[String, T]
    def wrapped: ShortNamed[V]
    def shortName = wrapped.shortName
  }

  trait Class[T] extends TokensReader[T] {
    def isLeftover = false
    def isFlag = false
    def companion: () => Any
    def main: MainData[T, Any]
  }

  def tryEither[T](f: => T) =
    try Right(f)
    catch { case e: Throwable => Left(e.toString) }

  implicit object FlagRead extends Flag
  implicit object StringRead extends Simple[String] {
    def shortName = "str"
    def read(strs: Seq[String]) = Right(strs.last)
  }
  implicit object BooleanRead extends Simple[Boolean] {
    def shortName = "bool"
    def read(strs: Seq[String]) = tryEither(strs.last.toBoolean)
  }
  implicit object ByteRead extends Simple[Byte] {
    def shortName = "byte"
    def read(strs: Seq[String]) = tryEither(strs.last.toByte)
  }
  implicit object ShortRead extends Simple[Short] {
    def shortName = "short"
    def read(strs: Seq[String]) = tryEither(strs.last.toShort)
  }
  implicit object IntRead extends Simple[Int] {
    def shortName = "int"
    def read(strs: Seq[String]) = tryEither(strs.last.toInt)
  }
  implicit object LongRead extends Simple[Long] {
    def shortName = "long"
    def read(strs: Seq[String]) = tryEither(strs.last.toLong)
  }
  implicit object FloatRead extends Simple[Float] {
    def shortName = "float"
    def read(strs: Seq[String]) = tryEither(strs.last.toFloat)
  }
  implicit object DoubleRead extends Simple[Double] {
    def shortName = "double"
    def read(strs: Seq[String]) = tryEither(strs.last.toDouble)
  }

  implicit def LeftoverRead[T: TokensReader.Simple]: TokensReader.ShortNamed[mainargs.Leftover[T]] =
    new LeftoverRead[T]()(implicitly[TokensReader.Simple[T]])

  class LeftoverRead[T](implicit val wrapped: TokensReader.Simple[T])
      extends Leftover[mainargs.Leftover[T], T] {
    def read(strs: Seq[String]) = {
      val (failures, successes) = strs
        .map(s =>
          implicitly[TokensReader[T]] match{
            case r: TokensReader.Simple[T] => r.read(Seq(s))
            case r: TokensReader.Leftover[T, _] => r.read(Seq(s))
          }
        )
        .partitionMap(identity)

      if (failures.nonEmpty) Left(failures.head)
      else Right(Leftover(successes: _*))
    }
  }

  implicit def OptionRead[T: TokensReader.Simple]: TokensReader[Option[T]] = new OptionRead[T]
  class OptionRead[T: TokensReader.Simple] extends Simple[Option[T]] {
    def shortName = implicitly[TokensReader.Simple[T]].shortName
    def read(strs: Seq[String]) = {
      strs.lastOption match {
        case None => Right(None)
        case Some(s) => implicitly[TokensReader.Simple[T]].read(Seq(s)) match {
            case Left(s) => Left(s)
            case Right(s) => Right(Some(s))
          }
      }
    }
    override def allowEmpty = true
  }

  implicit def SeqRead[C[_] <: Iterable[_], T: TokensReader.Simple](implicit
      factory: Factory[T, C[T]]
  ): TokensReader[C[T]] =
    new SeqRead[C, T]

  class SeqRead[C[_] <: Iterable[_], T: TokensReader.Simple](implicit factory: Factory[T, C[T]])
      extends Simple[C[T]] {
    def shortName = implicitly[TokensReader.Simple[T]].shortName
    def read(strs: Seq[String]) = {
      strs
        .foldLeft(Right(factory.newBuilder): Either[String, mutable.Builder[T, C[T]]]) {
          case (Left(s), _) => Left(s)
          case (Right(builder), token) =>
            implicitly[TokensReader.Simple[T]].read(Seq(token)) match {
              case Left(s) => Left(s)
              case Right(v) =>
                builder += v
                Right(builder)
            }
        }
        .map(_.result())
    }
    override def alwaysRepeatable = true
    override def allowEmpty = true
  }

  implicit def MapRead[K: TokensReader.Simple, V: TokensReader.Simple]: TokensReader[Map[K, V]] =
    new MapRead[K, V]
  class MapRead[K: TokensReader.Simple, V: TokensReader.Simple] extends Simple[Map[K, V]] {
    def shortName = "k=v"
    def read(strs: Seq[String]) = {
      strs.foldLeft[Either[String, Map[K, V]]](Right(Map())) {
        case (Left(s), _) => Left(s)
        case (Right(prev), token) =>
          token.split("=", 2) match {
            case Array(k, v) =>
              for {
                tuple <- Right((k, v)): Either[String, (String, String)]
                (k, v) = tuple
                key <- implicitly[TokensReader.Simple[K]].read(Seq(k))
                value <- implicitly[TokensReader.Simple[V]].read(Seq(v))
              } yield prev + (key -> value)

            case _ => Left("parameter must be in k=v format")
          }
      }
    }
    override def alwaysRepeatable = true
    override def allowEmpty = true
  }
}

object ArgSig {
  def create[T, B](name0: String, arg: mainargs.arg, defaultOpt: Option[B => T])
                  (implicit tokensReader: TokensReader[T]): ArgSig = {
    val nameOpt = scala.Option(arg.name).orElse(if (name0.length == 1 || arg.noDefaultName) None
    else Some(name0))
    val shortOpt = arg.short match {
      case '\u0000' => if (name0.length != 1 || arg.noDefaultName) None else Some(name0(0));
      case c => Some(c)
    }
    val docOpt = scala.Option(arg.doc)
    ArgSig(
      nameOpt,
      shortOpt,
      docOpt,
      defaultOpt.asInstanceOf[Option[Any => Any]],
      tokensReader,
      arg.positional
    )
  }

  def flatten[T](x: ArgSig): Seq[ArgSig] = x.reader match {
    case _: TokensReader.Terminal[T] => Seq(x)
    case cls: TokensReader.Class[_] => cls.main.argSigs0.flatMap(flatten(_))
  }
}

/**
 * Models what is known by the router about a single argument: that it has
 * a [[name]], a human-readable [[typeString]] describing what the type is
 * (just for logging and reading, not a replacement for a `TypeTag`) and
 * possible a function that can compute its default value
 */
case class ArgSig(
    name: Option[String],
    shortName: Option[Char],
    doc: Option[String],
    default: Option[Any => Any],
    reader: TokensReader[_],
    positional: Boolean
)

case class MethodMains[B](value: Seq[MainData[Any, B]], base: () => B)

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[flattenedArgSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[Invoker.invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class MainData[T, B](
    name: String,
    argSigs0: Seq[ArgSig],
    doc: Option[String],
    invokeRaw: (B, Seq[Any]) => T
) {

  val flattenedArgSigs: Seq[ArgSig] =
    argSigs0.iterator.flatMap[ArgSig](ArgSig.flatten(_)).toVector
}

object MainData {
  def create[T, B](
      methodName: String,
      main: mainargs.main,
      argSigs: Seq[ArgSig],
      invokeRaw: (B, Seq[Any]) => T
  ) = {
    MainData(
      Option(main.name).getOrElse(methodName),
      argSigs,
      Option(main.doc),
      invokeRaw
    )
  }
}
