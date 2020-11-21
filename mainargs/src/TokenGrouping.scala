package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String],
                            grouped: Map[ArgSig.Simple[_, B], Seq[String]])

object TokenGrouping{
  def groupArgs[B](flatArgs0: Seq[String],
                   argSigs0: Seq[ArgSig[_, B]],
                   allowPositional: Boolean,
                   allowRepeats: Boolean,
                   allowLeftover: Boolean): Result[TokenGrouping[B]] = {
    val argSigs: Seq[ArgSig.Simple[_, B]] = argSigs0
      .map(ArgSig.flatten(_).collect{case x: ArgSig.Simple[_, _] => x})
      .flatten


    val flatArgs = flatArgs0.toList
    val keywordArgMap = argSigs
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}
      .toMap[String, ArgSig.Simple[_, B]]

    @tailrec def rec(remaining: List[String],
                     current: Map[ArgSig.Simple[_, B], Vector[String]]): Result[TokenGrouping[B]] = {
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
                 current: Map[ArgSig.Simple[_, B], Vector[String]]): Result[TokenGrouping[B]] = {

      val duplicates = current
        .filter(x => x._2.size > 1 && !x._1.reader.alwaysRepeatable && !allowRepeats)
        .toSeq

      val missing = argSigs.filter(x =>
        !x.reader.allowEmpty &&
        x.default.isEmpty &&
        !current.contains(x) &&
        !x.flag
      )
      val unknown = if (allowLeftover) Nil else remaining
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
