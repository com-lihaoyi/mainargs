package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String], grouped: Map[ArgSig, Seq[String]])

object TokenGrouping {
  def groupArgs[B](
      flatArgs0: Seq[String],
      argSigs: Seq[(ArgSig, TokensReader.Terminal[_])],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      allowLeftover: Boolean
  ): Result[TokenGrouping[B]] = {
    val positionalArgSigs = argSigs.collect {
      case (a, r: TokensReader.Simple[_]) if allowPositional | a.positional =>
        a
    }

    val flatArgs = flatArgs0.toList
    val keywordArgMap = argSigs
      .collect {
        case (a, r: TokensReader.Simple[_]) if !a.positional => a
        case (a, r: TokensReader.Flag) => a
      }
      .flatMap { x => (x.name.map("--" + _) ++ x.shortName.map("-" + _)).map(_ -> x) }
      .toMap[String, ArgSig]

    @tailrec def rec(
        remaining: List[String],
        current: Map[ArgSig, Vector[String]]
    ): Result[TokenGrouping[B]] = {
      remaining match {
        case head :: rest =>

          if (head.startsWith("-") && head.exists(_ != '-')) {
            head.split("=", 2) match{
              case Array(first, second) =>
                keywordArgMap.get(first) match {
                  case Some(cliArg) if !cliArg.reader.isLeftover && !cliArg.reader.isFlag =>
                    rec(rest, Util.appendMap(current, cliArg, second))

                  case _ => complete(remaining, current)
                }

              case _ =>
                keywordArgMap.get(head) match {
                  case Some(cliArg) if cliArg.reader.isFlag =>
                    rec(rest, Util.appendMap(current, cliArg, ""))
                  case Some(cliArg) if !cliArg.reader.isLeftover =>
                    rest match {
                      case next :: rest2 => rec(rest2, Util.appendMap(current, cliArg, next))
                      case Nil =>
                        Result.Failure.MismatchedArguments(Nil, Nil, Nil, incomplete = Some(cliArg))
                    }
                  case _ => complete(remaining, current)
                }
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

      val missing = argSigs.collect {
        case (a, r: TokensReader.Simple[_])
          if !r.allowEmpty
          && a.default.isEmpty
          && !current.contains(a) =>
            a
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
