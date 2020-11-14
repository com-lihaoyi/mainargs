
package mainargs

import scala.annotation.switch

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
  def groupArgs(flatArgs: List[String]): Seq[(String, Option[String])] = {
    var keywordTokens = flatArgs
    var scriptArgs = Vector.empty[(String, Option[String])]

    while(keywordTokens.nonEmpty) keywordTokens match{
      case List(head, next, rest@_*) if head.startsWith("-") =>
        scriptArgs = scriptArgs :+ (head, Some(next))
        keywordTokens = rest.toList
      case List(head, rest@_*) =>
        scriptArgs = scriptArgs :+ (head, None)
        keywordTokens = rest.toList

    }
    scriptArgs
  }
  def stripDashes(s: String) = {
    if (s.startsWith("--")) s.drop(2)
    else if (s.startsWith("-")) s.drop(1)
    else s
  }

  def tryEither[T](t: => T, error: Throwable => Result.ParamError) = {
    try Right(t)
    catch{ case e: Throwable => Left(error(e))}
  }



  type FailMaybe = Either[Seq[Result.ParamError], Any]
  type FailAll = Either[Seq[Result.ParamError], Seq[Any]]

}