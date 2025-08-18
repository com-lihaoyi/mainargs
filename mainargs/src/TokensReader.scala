package mainargs
import scala.collection.compat._
import scala.collection.mutable

/**
 * Represents the ability to parse CLI input arguments into a type [[T]]
 *
 * Has a fixed number of direct subtypes - [[Simple]], [[Constant]], [[Flag]],
 * [[Leftover]], and [[Class]] - but each of those can be extended by an
 * arbitrary number of user-specified instances.
 */
sealed trait TokensReader[T] {
  def isLeftover = false
  def isFlag = false
  def isClass = false
  def isConstant = false
  def isSimple = false
}

object TokensReader {

  sealed trait Terminal[T] extends TokensReader[T]

  sealed trait ShortNamed[T] extends Terminal[T] {
    /**
     * The label that shows up in the CLI help message, e.g. the `bar` in
     * `--foo <bar>`
     */
    def shortName: String
  }

  /**
   * A [[TokensReader]] for a single CLI parameter that takes a value
   * e.g. `--foo bar`
   */
  trait Simple[T] extends ShortNamed[T] {
    /**
     * Converts the given input tokens to a [[T]] or an error `String`.
     * The input is a `Seq` because input tokens can be passed more than once,
     * e.g. `--foo bar --foo qux` will result in [[read]] being passed
     * `["foo", "qux"]`
     */
    def read(strs: Seq[String]): Either[String, T]

    /**
     * Whether is CLI param is repeatable
     */
    def alwaysRepeatable: Boolean = false

    /**
     * Whether this CLI param can be no passed from the CLI, even if a default
     * value is not specified. In that case, [[read]] receives an empty `Seq`
     */
    def allowEmpty: Boolean = false
    override def isSimple = true
  }

  /**
   * A [[TokensReader]] that doesn't read any tokens and just returns a value.
   * Useful sometimes for injecting things into main methods that aren't
   * strictly computed from CLI argument tokens but nevertheless need to get
   * passed in.
   */
  trait Constant[T] extends Terminal[T] {
    def read(): Either[String, T]
    override def isConstant = true
  }

  /**
   * A [[TokensReader]] for a flag that does not take any value, e.g. `--foo`
   */
  trait Flag extends Terminal[mainargs.Flag] {
    override def isFlag = true
  }

  /**
   * A [[TokensReader]] for parsing the left-over parameters that do not belong
   * to any other flag or parameter.
   */
  trait Leftover[T, V] extends ShortNamed[T] {
    def read(strs: Seq[String]): Either[String, T]

    def shortName: String
    override def isLeftover = true
  }

  /**
   * A [[TokensReader]] that can parse an instance of the class [[T]], which
   * may contain multiple fields each parsed by their own [[TokensReader]]
   */
  trait Class[T] extends TokensReader[T] {
    def companion: () => Any
    def main: MainData[T, Any]
    override def isClass = true
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
  implicit object BigDecimalRead extends Simple[BigDecimal] {
    def shortName = "bigdecimal"
    def read(strs: Seq[String]) = tryEither(BigDecimal(strs.last))
  }

  implicit def LeftoverRead[T: TokensReader.Simple]: TokensReader.Leftover[mainargs.Leftover[T], T] =
    new LeftoverRead[T]()(implicitly[TokensReader.Simple[T]])

  class LeftoverRead[T](implicit wrapped: TokensReader.Simple[T])
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
    def shortName = wrapped.shortName
  }

  implicit def OptionRead[T: TokensReader.Simple]: TokensReader.Simple[Option[T]] = new OptionRead[T]
  class OptionRead[T: TokensReader.Simple] extends Simple[Option[T]] {
    def shortName = implicitly[TokensReader.Simple[T]].shortName
    def read(strs: Seq[String]) = {
      if (implicitly[TokensReader.Simple[T]].alwaysRepeatable) {
        Option(strs).filter(_.nonEmpty) match{
          case None => Right(None)
          case Some(strs) => implicitly[TokensReader.Simple[T]].read(strs) match{
            case Left(s) => Left(s)
            case Right(s) => Right(Some(s))
          }
        }
      } else {
        strs.lastOption match{
          case None => Right(None)
          case Some(s) => implicitly[TokensReader.Simple[T]].read(Seq(s)) match{
            case Left(s) => Left(s)
            case Right(s) => Right(Some(s))
          }
        }
      }
    }
    override def alwaysRepeatable = implicitly[TokensReader.Simple[T]].alwaysRepeatable
    override def allowEmpty = true
  }

  implicit def SeqRead[C[_] <: Iterable[_], T: TokensReader.Simple](implicit
      factory: Factory[T, C[T]]
  ): TokensReader.Simple[C[T]] =
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

  implicit def MapRead[K: TokensReader.Simple, V: TokensReader.Simple]: TokensReader.Simple[Map[K, V]] =
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
    val shortOpt = arg.short match {
      case '\u0000' => if (name0.length != 1 || arg.noDefaultName) None else Some(name0(0));
      case c => Some(c)
    }

    val docOpt = scala.Option(arg.doc)
    new ArgSig(
      if (arg.noDefaultName || name0.length == 1) None else Some(name0),
      scala.Option(arg.name),
      shortOpt,
      docOpt,
      defaultOpt.asInstanceOf[Option[Any => Any]],
      tokensReader,
      arg.positional,
      arg.hidden
    )
  }

  def flatten[T](x: ArgSig): Seq[(ArgSig, TokensReader.Terminal[_])] = x.reader match {
    case r: TokensReader.Terminal[T] => Seq((x, r))
    case cls: TokensReader.Class[_] => cls.main.argSigs0.flatMap(flatten(_))
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def apply(unMappedName: Option[String],
            shortName: Option[Char],
            doc: Option[String],
            default: Option[Any => Any],
            reader: TokensReader[_],
            positional: Boolean,
            hidden: Boolean) = {

    new ArgSig(unMappedName, unMappedName, shortName, doc, default, reader, positional, hidden)
  }

  def unapply(a: ArgSig) = Option(
    (a.unMappedName, a.shortName, a.doc, a.default, a.reader, a.positional, a.hidden)
  )
}

/**
 * Models what is known by the router about a single argument: that it has
 * a [[longName]], a human-readable [[typeString]] describing what the type is
 * (just for logging and reading, not a replacement for a `TypeTag`) and
 * possible a function that can compute its default value
 */
class ArgSig private[mainargs] (val defaultLongName: Option[String],
                                val argName: Option[String],
                                val shortName: Option[Char],
                                val doc: Option[String],
                                val default: Option[Any => Any],
                                val reader: TokensReader[_],
                                val positional: Boolean,
                                val hidden: Boolean
) extends Product with Serializable with Equals{
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def name = defaultLongName
  override def canEqual(that: Any): Boolean = true

  override def hashCode(): Int = ArgSig.unapply(this).hashCode()
  override def equals(o: Any): Boolean = o match {
    case other: ArgSig => ArgSig.unapply(this) == ArgSig.unapply(other)
    case _ => false
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def this(unmappedName: Option[String],
           shortName: Option[Char],
           doc: Option[String],
           default: Option[Any => Any],
           reader: TokensReader[_],
           positional: Boolean,
           hidden: Boolean) = {
    this(unmappedName, unmappedName, shortName, doc, default, reader, positional, hidden)
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def copy(unMappedName: Option[String] = this.unMappedName,
           shortName: Option[Char] = this.shortName,
           doc: Option[String] = this.doc,
           default: Option[Any => Any] = this.default,
           reader: TokensReader[_] = this.reader,
           positional: Boolean = this.positional,
           hidden: Boolean = this.hidden) = {
    ArgSig(unMappedName, shortName, doc, default, reader, positional, hidden)
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def productArity = 9
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def productElement(n: Int) = n match{
    case 0 => defaultLongName
    case 1 => argName
    case 2 => shortName
    case 3 => doc
    case 4 => default
    case 5 => reader
    case 6 => positional
    case 7 => hidden
  }

  def unMappedName: Option[String] = argName.orElse(defaultLongName)
  def longName(nameMapper: String => Option[String]): Option[String] = argName.orElse(mappedName(nameMapper)).orElse(defaultLongName)
  def mappedName(nameMapper: String => Option[String]): Option[String] =
    if (argName.isDefined) None else defaultLongName.flatMap(nameMapper)
}


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
class MainData[T, B] private[mainargs] (
    val mainName: Option[String],
    val defaultName: String,
    val argSigs0: Seq[ArgSig],
    val doc: Option[String],
    val invokeRaw: (B, Seq[Any]) => T
) extends Product with Serializable with Equals{
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def name = mainName.getOrElse(defaultName)
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def productArity = 5
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def productElement(n: Int) = n match{
    case 0 => mainName
    case 1 => defaultName
    case 2 => argSigs0
    case 3 => doc
    case 4 => invokeRaw
  }
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def copy(name: String = this.unmappedName,
           argSigs0: Seq[ArgSig] = this.argSigs0,
           doc: Option[String] = this.doc,
           invokeRaw: (B, Seq[Any]) => T = this.invokeRaw) = MainData(
    name, argSigs0, doc, invokeRaw
  )
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def this(name: String,
           argSigs0: Seq[ArgSig],
           doc: Option[String],
           invokeRaw: (B, Seq[Any]) => T) = this(
    Some(name), name, argSigs0, doc, invokeRaw
  )
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  override def hashCode(): Int = MainData.unapply(this).hashCode()

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  override def equals(obj: Any): Boolean = obj match{
    case x: MainData[_, _] => MainData.unapply(x) == MainData.unapply(this)
    case _ => false
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  override def canEqual(that: Any): Boolean = true

  def unmappedName: String = mainName.getOrElse(defaultName)

  def name(nameMapper: String => Option[String]) = mainName.orElse(mappedName(nameMapper)).getOrElse(defaultName)
  def mappedName(nameMapper: String => Option[String]): Option[String] =
    if (mainName.isDefined) None
    else nameMapper(defaultName)

  val flattenedArgSigs: Seq[(ArgSig, TokensReader.Terminal[_])] =
    argSigs0.iterator.flatMap[(ArgSig, TokensReader.Terminal[_])](ArgSig.flatten(_)).toVector

  val renderedArgSigs: Seq[ArgSig] =
    flattenedArgSigs.collect{case (a, r) if !a.hidden && !r.isConstant => a}
}

object MainData {
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def unapply[T, B](x: MainData[T, B]) = Option((x.mainName, x.defaultName, x.argSigs0, x.doc, x.invokeRaw))
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def apply[T, B](name: String,
                  argSigs0: Seq[ArgSig],
                  doc: Option[String],
                  invokeRaw: (B, Seq[Any]) => T) = {
    new MainData(Some(name), name, argSigs0, doc, invokeRaw)
  }
  def create[T, B](
      methodName: String,
      main: mainargs.main,
      argSigs: Seq[ArgSig],
      invokeRaw: (B, Seq[Any]) => T
  ) = {
    new MainData(
      Option(main.name),
      methodName,
      argSigs,
      Option(main.doc),
      invokeRaw
    )
  }
}
