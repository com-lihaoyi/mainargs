package mainargs

class Read[T](val read: (Option[T], String) => Either[String, T], default0: => Option[T] = None){
  def default = default0
}
object Read{
  def tryEither[T](f: => T) = try Right(f) catch{case e: Throwable => Left(e.toString)}
  implicit def OptionRead[T: Read] = new Read[Option[T]](
    (prev, s) => implicitly[Read[T]].read(implicitly[Read[T]].default, s).map(Some(_))
  )
  implicit def SeqRead[T: Read] = new Read[Seq[T]](
    (prev, s) =>
      implicitly[Read[T]].read(implicitly[Read[T]].default, s).map(prev.getOrElse(Vector()) :+ _),
    default0 = Some(Vector())
  )
  implicit object StringRead extends Read[String]((prev, s) => Right(s))
  implicit object BooleanRead extends Read[Boolean]((prev, s) => tryEither(s.toBoolean))
  implicit object ByteRead extends Read[Byte]((prev, s) => tryEither(s.toByte))
  implicit object ShortRead extends Read[Short]((prev, s) => tryEither(s.toShort))
  implicit object IntRead extends Read[Int]((prev, s) => tryEither(s.toInt))
  implicit object LongRead extends Read[Long]((prev, s) => tryEither(s.toLong))
  implicit object FloatRead extends Read[Float]((prev, s) => tryEither(s.toFloat))
  implicit object DoubleRead extends Read[Double]((prev, s) => tryEither(s.toDouble))

}
