package mainargs

import scala.annotation.tailrec

case class TokenGrouping[B](remaining: List[String], grouped: Map[ArgSig, Seq[String]])

object TokenGrouping {
  @deprecated("Binary Compatibility Shim")
  def groupArgs[B](
      flatArgs0: Seq[String],
      argSigs: Seq[(ArgSig, TokensReader.Terminal[_])],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      allowLeftover: Boolean,
  ): Result[TokenGrouping[B]] = {
    groupArgs(flatArgs0, argSigs, allowPositional, allowRepeats, allowLeftover, _ => None)
  }

  def groupArgs[B](
      flatArgs0: Seq[String],
      argSigs: Seq[(ArgSig, TokensReader.Terminal[_])],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      allowLeftover: Boolean,
      nameMapper: String => Option[String]
  ): Result[TokenGrouping[B]] = {
    val positionalArgSigs: Seq[ArgSig] = argSigs.collect {
      case (a, r: TokensReader.Simple[_]) if allowPositional | a.positional =>
        a
    }

    val flatArgs = flatArgs0.toList
    def makeKeywordArgMap(getNames: ArgSig => Iterable[String]): Map[String, ArgSig] = argSigs
      .collect {
        case (a, r: TokensReader.Simple[_]) if !a.positional => a
        case (a, r: TokensReader.Flag) => a
      }
      .flatMap { x => getNames(x).map(_ -> x) }
      .toMap[String, ArgSig]

    lazy val keywordArgMap = makeKeywordArgMap(
      x => x.mappedName(nameMapper).map("--"+ _ ) ++ x.longName(Util.nullNameMapper).map("--" + _) ++ x.shortName.map("-" + _)
    )

    lazy val longKeywordArgMap = makeKeywordArgMap(
      x => x.mappedName(nameMapper).map("--"+ _ ) ++ x.longName(Util.nullNameMapper).map("--" + _)
    )

    @tailrec def rec(
        remaining: List[String],
        current: Map[ArgSig, Vector[String]]
    ): Result[TokenGrouping[B]] = {
      remaining match {
        case head :: rest =>

          def lookupArgMap(k: String, m: Map[String, ArgSig]): Option[(ArgSig, mainargs.TokensReader[_])] = {
            m.get(k).map(a => (a, a.reader))
          }

          if (head.startsWith("-") && head.exists(_ != '-')) {
            head.split("=", 2) match{
              case Array(first, second) =>
                lookupArgMap(first, longKeywordArgMap) match {
                  case Some((cliArg, _: TokensReader.Simple[_])) =>
                    rec(rest, Util.appendMap(current, cliArg, second))

                  case _ => complete(remaining, current)
                }

              case _ =>
                lookupArgMap(head, keywordArgMap) match {
                  case Some((cliArg, _: TokensReader.Flag)) =>
                    rec(rest, Util.appendMap(current, cliArg, ""))
                  case Some((cliArg, _: TokensReader.Simple[_])) =>
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
