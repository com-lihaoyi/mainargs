
package mainargs

object Util{

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