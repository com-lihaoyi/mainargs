package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String], grouped: Map[ArgSig, Seq[String]])

object TokenGrouping {
  def groupArgs[B](
      flatArgs0: Seq[String],
      argSigs0: Seq[ArgSig],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      allowLeftover: Boolean
  ): Result[TokenGrouping[B]] = {
    val argSigs: Seq[ArgSig] = argSigs0
      .map(ArgSig.flatten(_).collect { case x: ArgSig => x })
      .flatten

    val positionalArgSigs = argSigs
      .filter {
        case x: ArgSig if x.reader.isLeftover || x.reader.isConstant => false
        case x: ArgSig if x.positional => true
        case x => allowPositional
      }

    val flatArgs = flatArgs0.toList
    val keywordArgMap = argSigs
      .filter { case x: ArgSig if x.positional => false; case _ => true }
      .flatMap { x => (x.name.map("--" + _) ++ x.shortName.map("-" + _)).map(_ -> x) }
      .toMap[String, ArgSig]

    @tailrec def rec(
        remaining: List[String],
        current: Map[ArgSig, Vector[String]]
    ): Result[TokenGrouping[B]] = {
      remaining match {
        case head :: rest =>
          if (head.startsWith("-") && head.exists(_ != '-')) {
            keywordArgMap.get(head) match {
              case Some(cliArg: ArgSig) if cliArg.reader.isFlag =>
                rec(rest, Util.appendMap(current, cliArg, ""))
              case Some(cliArg: ArgSig) if !cliArg.reader.isLeftover =>
                rest match {
                  case next :: rest2 => rec(rest2, Util.appendMap(current, cliArg, next))
                  case Nil =>
                    Result.Failure.MismatchedArguments(Nil, Nil, Nil, incomplete = Some(cliArg))
                }

              case _ => complete(remaining, current)
            }
          } else {
            positionalArgSigs.find(!current.contains(_)) match {
              case Some(nextInLine) => rec(rest, Util.appendMap(current, nextInLine, head))
              case None => complete(remaining, current)
            }
          }

        case _ => complete(remaining, current)
      }
    }

    def complete(
        remaining: List[String],
        current: Map[ArgSig, Vector[String]]
    ): Result[TokenGrouping[B]] = {

      val duplicates = current
        .filter {
          case (a: ArgSig, vs) =>
            a.reader match {
              case r: TokensReader.Flag => vs.size > 1 && !allowRepeats
              case r: TokensReader.Simple[_] => vs.size > 1 && !r.alwaysRepeatable && !allowRepeats
              case r: TokensReader.Leftover[_, _] => false
              case r: TokensReader.Constant[_] => false
            }

        }
        .toSeq

      val missing = argSigs
        .collect { case x: ArgSig => x }
        .filter { x =>
          x.reader match {
            case r: TokensReader.Simple[_] =>
              !r.allowEmpty &&
              x.default.isEmpty &&
              !current.contains(x)
            case _ => false
          }
        }

      val unknown = if (allowLeftover) Nil else remaining
      if (missing.nonEmpty || duplicates.nonEmpty || unknown.nonEmpty) {
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
