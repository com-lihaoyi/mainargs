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
    def makeKeywordArgMap(getNames: ArgSig => Iterable[String]) = argSigs
      .collect {
        case (a, r: TokensReader.Simple[_]) if !a.positional => a
        case (a, r: TokensReader.Flag) => a
      }
      .flatMap { x => getNames(x).map(_ -> x) }
      .toMap[String, ArgSig]

    lazy val shortArgMap: Map[Char, ArgSig] = argSigs
      .collect{case (a, _) => a.shortName.map(_ -> a)}
      .flatten
      .toMap[Char, ArgSig]

    lazy val shortFlagArgMap: Map[Char, ArgSig] = argSigs
      .collect{case (a, r: TokensReader.Flag) => a.shortName.map(_ -> a)}
      .flatten
      .toMap[Char, ArgSig]

    lazy val keywordArgMap = makeKeywordArgMap(
      x => x.name.map("--" + _) ++ x.shortName.map("-" + _)
    )

    lazy val longKeywordArgMap = makeKeywordArgMap(x => x.name.map("--" + _))

    @tailrec def rec(
        remaining: List[String],
        current: Map[ArgSig, Vector[String]]
    ): Result[TokenGrouping[B]] = {
      remaining match {
        case head :: rest =>

          def lookupArgMap(k: String, m: Map[String, ArgSig]): Option[(ArgSig, mainargs.TokensReader[_])] = {
            m.get(k).map(a => (a, a.reader))
          }


          // special handling for combined short args of the style "-xvf" or "-j10"
          if (head.startsWith("-") &&
            head.lift(1).exists(c => c != '-' && c != '=') &&
            head.lift(2).exists(c => c != '-' && c != '=')){
            val chars = head.drop(1)
            var rest2 = rest
            var i = 0
            var currentMap = current
            var failure: Result.Failure = null

            while (i < chars.length){
              val c = chars(i)
              shortFlagArgMap.get(c) match{
                case Some(a) =>
                  // For `Flag`s in chars, we consume the char, set it to `true`, and continue
                  currentMap = Util.appendMap(currentMap, a, "")
                  i += 1
                case None =>
                  // For other kinds of short arguments, we consume the char, set the value to
                  // the remaining characters, and exit
                  shortArgMap.get(c) match{
                    case Some(a) =>
                      if (i == chars.length - 1) {
                        rest2 match{
                          case Nil =>
                            failure = Result.Failure.MismatchedArguments(incomplete = Some(a))
                          case next :: remaining =>
                            currentMap = Util.appendMap(currentMap, a, next)
                            rest2 = remaining
                        }
                      }else {
                        currentMap = Util.appendMap(currentMap, a, chars.drop(i + 1))
                      }
                    case None =>
                      println("C")
                      // If we encounter a character that is neither a short flag or a
                      // short argument, it is an error
                      failure = Result.Failure.MismatchedArguments(unknown = Seq(c.toString))
                  }
                  i = chars.length
              }

            }

            if (failure != null) failure
            else rec(rest2, currentMap)

          } else if (head.startsWith("-") && head.exists(_ != '-')) {
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
