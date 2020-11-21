
package mainargs

import scala.annotation.{switch, tailrec}

object Util{
  def literalize(s: IndexedSeq[Char], unicode: Boolean = false) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')

    sb.result()
  }

  def stripDashes(s: String) = {
    if (s.startsWith("--")) s.drop(2)
    else if (s.startsWith("-")) s.drop(1)
    else s
  }

  def tryEither[T](t: => T,
                   error: Throwable => Result.ParamError): Either[Result.ParamError, T] = {
    try Right(t)
    catch{ case e: Throwable => Left(error(e))}
  }


  def appendMap[K, V](current: Map[K, Vector[V]], k: K, v: V): Map[K, Vector[V]] = {
    if(current.contains(k)) current + (k -> (current(k) :+ v))
    else current + (k -> Vector(v))
  }



}

/**
 * A simple box to make passing around `Any`s less dangerous
 */
case class Computed[+T](value: T)