package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String],
                            grouped: Map[ArgSig.Named[_, B], Seq[String]])

object TokenGrouping{
  def groupArgs[B](flatArgs0: Seq[String],
                   argSigs0: Seq[ArgSig[_, B]],
                   allowPositional: Boolean,
                   allowRepeats: Boolean,
                   allowLeftover: Boolean): Result[TokenGrouping[B]] = {
    val argSigs: Seq[ArgSig.Named[_, B]] = argSigs0
      .map(ArgSig.flatten(_).collect{case x: ArgSig.Named[_, _] => x})
      .flatten


    val positionalArgSigs = argSigs
      .filter{
        case x: ArgSig.Simple[_, _] if x.reader.noTokens => false
        case x: ArgSig.Simple[_, _] if x.positional => true
        case x => allowPositional
      }

    val flatArgs = flatArgs0.toList
    val keywordArgMap = argSigs
      .filter{case x: ArgSig.Simple[_, _] if x.positional => false; case _ => true}
      .flatMap{x => (x.name.map("--" + _) ++ x.shortName.map("-" + _)).map(_ -> x)}
      .toMap[String, ArgSig.Named[_, B]]

    @tailrec def rec(remaining: List[String],
                     current: Map[ArgSig.Named[_, B], Vector[String]]): Result[TokenGrouping[B]] = {
      remaining match{
        case head :: rest  =>
          if (head.startsWith("-")){
            keywordArgMap.get(head) match {
              case Some(cliArg: ArgSig.Flag[_]) =>
                rec(rest, Util.appendMap(current, cliArg, ""))
              case Some(cliArg: ArgSig.Simple[_, _]) =>
                rest match{
                  case next :: rest2 => rec(rest2, Util.appendMap(current, cliArg, next))
                  case Nil => Result.Failure.MismatchedArguments(Nil, Nil, Nil, incomplete = Some(cliArg))
                }

              case None => complete(remaining, current)
            }
          }else {
            positionalArgSigs.find(!current.contains(_)) match{
              case Some(nextInLine) => rec(rest, Util.appendMap(current, nextInLine, head))
              case None => complete(remaining, current)
            }
          }

        case _ => complete(remaining, current)

      }
    }
    def complete(remaining: List[String],
                 current: Map[ArgSig.Named[_, B], Vector[String]]): Result[TokenGrouping[B]] = {

      val duplicates = current
        .filter{
          case (a: ArgSig.Flag[_], vs) => vs.size > 1 && !allowRepeats
          case (a: ArgSig.Simple[_, _], vs) =>
            vs.size > 1 && !a.reader.alwaysRepeatable && !allowRepeats
        }
        .toSeq

      val missing = argSigs
        .collect{ case x: ArgSig.Simple[_, _] => x}
        .filter{ x =>
          !x.reader.allowEmpty &&
          x.default.isEmpty &&
          !current.contains(x)
        }
      val unknown = if (allowLeftover) Nil else remaining
      if (missing.nonEmpty || duplicates.nonEmpty || unknown.nonEmpty){
        Result.Failure.MismatchedArguments(
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
