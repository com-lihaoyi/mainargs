package mainargs

class ArgParser[T](val shortName: String,
                   val read: (Option[T], String) => Either[String, T],
                   default0: => Option[T] = None,
                   val alwaysRepeatable: Boolean = false){
  def default = default0
}
object ArgParser{
  def tryEither[T](f: => T) = try Right(f) catch{case e: Throwable => Left(e.toString)}
  implicit def OptionRead[T: ArgParser] = new ArgParser[Option[T]](
    implicitly[ArgParser[T]].shortName,
    (prev, s) => implicitly[ArgParser[T]].read(implicitly[ArgParser[T]].default, s).map(Some(_)),
    default0 = Some(None)
  )
  implicit def SeqRead[T: ArgParser] = new ArgParser[Seq[T]](
    implicitly[ArgParser[T]].shortName,
    (prev, s) =>
      implicitly[ArgParser[T]].read(implicitly[ArgParser[T]].default, s).map(prev.getOrElse(Vector()) :+ _),
    default0 = Some(Vector()),
    alwaysRepeatable = true
  )
  implicit object StringRead extends ArgParser[String]("str", (prev, s) => Right(s))
  implicit object BooleanRead extends ArgParser[Boolean]("bool", (prev, s) => tryEither(s.toBoolean))
  implicit object ByteRead extends ArgParser[Byte]("byte", (prev, s) => tryEither(s.toByte))
  implicit object ShortRead extends ArgParser[Short]("short", (prev, s) => tryEither(s.toShort))
  implicit object IntRead extends ArgParser[Int]("int", (prev, s) => tryEither(s.toInt))
  implicit object LongRead extends ArgParser[Long]("long", (prev, s) => tryEither(s.toLong))
  implicit object FloatRead extends ArgParser[Float]("float", (prev, s) => tryEither(s.toFloat))
  implicit object DoubleRead extends ArgParser[Double]("double", (prev, s) => tryEither(s.toDouble))

}
