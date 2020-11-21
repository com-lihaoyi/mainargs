package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String],
                            grouped: Map[ArgSig[_, B], Seq[String]])

object TokenGrouping{
  def groupArgs[B](flatArgs0: Seq[String],
                   argSigs0: Seq[AnyArgSig[_, B]],
                   allowPositional: Boolean,
                   allowRepeats: Boolean): Result[TokenGrouping[B]] = {
    val argSigs: Seq[ArgSig[_, B]] = argSigs0.map(AnyArgSig.flatten(_)).flatten

    val flatArgs = flatArgs0.toList
    val keywordArgMap = argSigs
      .filter(!_.varargs) // varargs can only be passed positionally
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}
      .toMap[String, ArgSig[_, B]]

    @tailrec def rec(remaining: List[String],
                     current: Map[ArgSig[_, B], Vector[String]]): Result[TokenGrouping[B]] = {
      remaining match{
        case head :: rest  =>
          if (head.startsWith("-")){
            keywordArgMap.get(if(head.startsWith("--")) head.drop(2) else head.drop(1)) match {
              case Some(cliArg) =>
                if (cliArg.flag) rec(rest, current + (cliArg -> Vector()))
                else rest match{
                  case next :: rest2 => rec(rest2, Util.appendMap(current, cliArg, next))
                  case Nil => Result.Error.MismatchedArguments(Nil, Nil, Nil, incomplete = Some(cliArg))
                }

              case None => complete(remaining, current)
            }
          }else if (allowPositional){

            keywordArgMap.values.find(as => !current.exists(_._1 == as)) match{
              case Some(nextInLine) => rec(rest, Util.appendMap(current, nextInLine, head))
              case None => complete(remaining, current)
            }
          } else complete(remaining, current)

        case _ => complete(remaining, current)

      }
    }
    def complete(remaining: List[String],
                 current: Map[ArgSig[_, B], Vector[String]]): Result[TokenGrouping[B]] = {

      val duplicates = current
        .filter(x => x._2.size > 1 && !x._1.reader.alwaysRepeatable && !allowRepeats)
        .toSeq

      val missing = argSigs.filter(x =>
        !x.reader.allowEmpty &&
        x.default.isEmpty &&
        !current.contains(x) &&
        !x.varargs &&
        !x.flag
      )
      val unknown = if (argSigs.exists(_.varargs)) Nil else remaining
      if (missing.nonEmpty || duplicates.nonEmpty || unknown.nonEmpty){
        Result.Error.MismatchedArguments(
          missing = missing,
          unknown = unknown,
          duplicate = duplicates,
          incomplete = None
        )
      } else Result.Success(TokenGrouping(remaining, current))

    }
    rec(flatArgs, Map())
  }
}
