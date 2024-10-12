package mainargs

import scala.annotation.{switch, tailrec}

object Util {
  def nullNameMapper(s: String): Option[String] = None

  def kebabCaseNameMapper(s: String): Option[String] = {
    baseNameMapper(s, '-')
  }
  def snakeCaseNameMapper(s: String): Option[String] = {
    baseNameMapper(s, '_')
  }

  def baseNameMapper(s: String, sep: Char): Option[String] = {
    val chars = new collection.mutable.StringBuilder
    // 'D' -> digit
    // 'U' -> uppercase
    // 'L' -> lowercase
    // 'O' -> other
    var state = ' '

    for (c <- s) {
      if (c.isDigit) {
        if (state == 'L' || state == 'U') chars.append(sep)
        chars.append(c)
        state = 'D'
      } else if (c.isUpper) {
        if (state == 'L' || state == 'D') chars.append(sep)
        chars.append(c.toLower)
        state = 'U'
      } else if (c.isLower) {
        chars.append(c)
        state = 'L'
      } else {
        state = 'O'
        chars.append(c)
      }
    }

    Some(chars.toString())
  }

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

  def appendMap[K, V](current: Map[K, Vector[V]], k: K, v: V): Map[K, Vector[V]] = {
    if (current.contains(k)) current + (k -> (current(k) :+ v))
    else current + (k -> Vector(v))
  }
}
