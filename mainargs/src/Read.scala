package mainargs

class Read[T](val read: (Option[T], String) => T, default0: => Option[T] = None){
  def default = default0
}
object Read{
  implicit def OptionRead[T: Read] = new Read[Option[T]](
    (prev, s) => Some(implicitly[Read[T]].read(implicitly[Read[T]].default, s))
  )
  implicit def SeqRead[T: Read] = new Read[Seq[T]](
    (prev, s) => prev.getOrElse(Vector()) :+ implicitly[Read[T]].read(implicitly[Read[T]].default, s),
    default0 = Some(Vector())
  )
  implicit object StringRead extends Read[String]((prev, s) => s)
  implicit object BooleanRead extends Read[Boolean]((prev, s) => s.toBoolean)
  implicit object ByteRead extends Read[Byte]((prev, s) => s.toByte)
  implicit object ShortRead extends Read[Short]((prev, s) => s.toShort)
  implicit object IntRead extends Read[Int]((prev, s) => s.toInt)
  implicit object LongRead extends Read[Long]((prev, s) => s.toLong)
  implicit object FloatRead extends Read[Float]((prev, s) => s.toFloat)
  implicit object DoubleRead extends Read[Double]((prev, s) => s.toDouble)

}
