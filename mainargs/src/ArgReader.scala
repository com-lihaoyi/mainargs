package mainargs
import scala.collection.compat._
import scala.collection.mutable
class ArgReader[T](val shortName: String,
                   val read: Seq[String] => Either[String, T],
                   val alwaysRepeatable: Boolean = false,
                   val allowEmpty: Boolean = false)
object ArgReader{
  def tryEither[T](f: => T) = try Right(f) catch{case e: Throwable => Left(e.toString)}

  implicit object StringRead extends ArgReader[String]("str", strs => Right(strs.last))
  implicit object BooleanRead extends ArgReader[Boolean]("bool", strs => tryEither(strs.last.toBoolean))
  implicit object ByteRead extends ArgReader[Byte]("byte", strs => tryEither(strs.last.toByte))
  implicit object ShortRead extends ArgReader[Short]("short", strs => tryEither(strs.last.toShort))
  implicit object IntRead extends ArgReader[Int]("int", strs => tryEither(strs.last.toInt))
  implicit object LongRead extends ArgReader[Long]("long", strs => tryEither(strs.last.toLong))
  implicit object FloatRead extends ArgReader[Float]("float", strs => tryEither(strs.last.toFloat))
  implicit object DoubleRead extends ArgReader[Double]("double", strs => tryEither(strs.last.toDouble))

  implicit def OptionRead[T: ArgReader] = new ArgReader[Option[T]](
    implicitly[ArgReader[T]].shortName,
    strs => {
      strs.lastOption match{
        case None => Right(None)
        case Some(s) => implicitly[ArgReader[T]].read(Seq(s)) match{
          case Left(s) =>Left(s)
          case Right(s) => Right(Some(s))
        }
      }
    },
    allowEmpty = true
  )
  implicit def SeqRead[C[_] <: Iterable[_], T: ArgReader](implicit factory: Factory[T, C[T]]) = new ArgReader[C[T]](
    implicitly[ArgReader[T]].shortName,
    strs => {
      strs
        .foldLeft(Right(factory.newBuilder): Either[String, mutable.Builder[T, C[T]]]){
          case (Left(s), _) => Left(s)
          case (Right(builder), token) =>
            implicitly[ArgReader[T]].read(Seq(token)) match{
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
  implicit def MapRead[K: ArgReader, V: ArgReader] = new ArgReader[Map[K, V]](
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
                key <- implicitly[ArgReader[K]].read(Seq(k))
                value <- implicitly[ArgReader[V]].read(Seq(v))
              }yield prev + (key -> value)

            case _ => Left("parameter must be in k=v format")
          }
      }
    }
  )
}
