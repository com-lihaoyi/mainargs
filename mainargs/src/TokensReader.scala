package mainargs
import scala.collection.compat._
import scala.collection.mutable
class TokensReader[T](val shortName: String,
                      val read: Seq[String] => Either[String, T],
                      val alwaysRepeatable: Boolean = false,
                      val allowEmpty: Boolean = false,
                      val noTokens: Boolean = false)
object TokensReader{
  def tryEither[T](f: => T) = try Right(f) catch{case e: Throwable => Left(e.toString)}

  implicit object StringRead extends TokensReader[String]("str", strs => Right(strs.last))
  implicit object BooleanRead extends TokensReader[Boolean]("bool", strs => tryEither(strs.last.toBoolean))
  implicit object ByteRead extends TokensReader[Byte]("byte", strs => tryEither(strs.last.toByte))
  implicit object ShortRead extends TokensReader[Short]("short", strs => tryEither(strs.last.toShort))
  implicit object IntRead extends TokensReader[Int]("int", strs => tryEither(strs.last.toInt))
  implicit object LongRead extends TokensReader[Long]("long", strs => tryEither(strs.last.toLong))
  implicit object FloatRead extends TokensReader[Float]("float", strs => tryEither(strs.last.toFloat))
  implicit object DoubleRead extends TokensReader[Double]("double", strs => tryEither(strs.last.toDouble))

  implicit def OptionRead[T: TokensReader]: TokensReader[Option[T]] = new TokensReader[Option[T]](
    implicitly[TokensReader[T]].shortName,
    strs => {
      if (implicitly[TokensReader[T]].alwaysRepeatable) {
        Option(strs).filter(_.nonEmpty) match{
          case None => Right(None)
          case Some(strs) => implicitly[TokensReader[T]].read(strs) match{
            case Left(s) => Left(s)
            case Right(s) => Right(Some(s))
          }
        }
      } else {
        strs.lastOption match{
          case None => Right(None)
          case Some(s) => implicitly[TokensReader[T]].read(Seq(s)) match{
            case Left(s) => Left(s)
            case Right(s) => Right(Some(s))
          }
        }
      }
    },
    alwaysRepeatable = implicitly[TokensReader[T]].alwaysRepeatable,
    allowEmpty = true
  )
  implicit def SeqRead[C[_] <: Iterable[_], T: TokensReader](implicit factory: Factory[T, C[T]]): TokensReader[C[T]] = new TokensReader[C[T]](
    implicitly[TokensReader[T]].shortName,
    strs => {
      strs
        .foldLeft(Right(factory.newBuilder): Either[String, mutable.Builder[T, C[T]]]){
          case (Left(s), _) => Left(s)
          case (Right(builder), token) =>
            implicitly[TokensReader[T]].read(Seq(token)) match{
              case Left(s) => Left(s)
              case Right(v) =>
                builder += v
                Right(builder)
            }
        }
        .map(_.result())
    },
    alwaysRepeatable = true,
    allowEmpty = true
  )
  implicit def MapRead[K: TokensReader, V: TokensReader]: TokensReader[Map[K, V]] = new TokensReader[Map[K, V]](
    "k=v",
    strs => {
      strs.foldLeft[Either[String, Map[K, V]]](Right(Map())){
        case (Left(s), _) => Left(s)
        case (Right(prev), token) =>
          token.split("=", 2) match{
            case Array(k, v) =>
              for {
                tuple <- Right((k, v)): Either[String, (String, String)]
                (k, v) = tuple
                key <- implicitly[TokensReader[K]].read(Seq(k))
                value <- implicitly[TokensReader[V]].read(Seq(v))
              }yield prev + (key -> value)

            case _ => Left("parameter must be in k=v format")
          }
      }
    },
    alwaysRepeatable = true,
    allowEmpty = true
  )
}
