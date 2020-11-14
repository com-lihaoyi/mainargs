package mainargs


import scala.annotation.tailrec

case class Grouping[T](grouped: List[(ArgSig[T], Option[String])],
                       remaining: List[String])
object Grouping{
  def groupArgs[T](flatArgs: List[String],
                   argSigs: Seq[ArgSig[T]]): Either[String, Grouping[T]] = {

    val argsMap0: Seq[(String, ArgSig[T])] = argSigs
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}

    val argsMap = argsMap0.toMap

    @tailrec def rec(keywordTokens: List[String],
                     current: Grouping[T]): Either[String, Grouping[T]] = {
      keywordTokens match{
        case head :: rest if head(0) == '-' =>
          val realName = if(head(1) == '-') head.drop(2) else head.drop(1)

          argsMap.get(realName) match {
            case Some(cliArg) =>
              if (cliArg.flag) {
                rec(rest, Grouping((cliArg -> None) :: current.grouped, Nil))
              } else rest match{
                case next :: rest2 =>
                  rec(rest2, Grouping((cliArg -> Some(next)) :: current.grouped, Nil))
                case Nil => Left(s"Expected a value after argument $head")
              }

            case None => Right(Grouping(current.grouped, keywordTokens))
          }

        case _ => Right(Grouping(current.grouped, keywordTokens))

      }
    }
    rec(flatArgs, Grouping(Nil, Nil))
  }
}