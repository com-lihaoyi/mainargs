package mainargs


import scala.annotation.tailrec

case class Grouping[T](remaining: List[String],
                       grouped: Map[ArgSig[T], String])
object Grouping{
  def groupArgs[T](flatArgs: List[String],
                   argSigs: Seq[ArgSig[T]],
                   allowPositional: Boolean): Result[Grouping[T]] = {

    val keywordArgMap = argSigs
      .filter(!_.varargs) // varargs can only be passed positionally
      .flatMap{x => Seq(x.name -> x) ++ x.shortName.map(_.toString -> x)}
      .toMap

    def appendMap[K, V](current: Map[K, Vector[V]], k: K, v: V): Map[K, Vector[V]] = {
      if(current.contains(k)) current + (k -> (current(k) :+ v))
      else current + (k -> Vector(v))
    }
    @tailrec def rec(remaining: List[String],
                     current: Map[ArgSig[T], Vector[String]]): Result[Grouping[T]] = {
      remaining match{
        case head :: rest  =>
          if (head.startsWith("-")){
            keywordArgMap.get(if(head.startsWith("--")) head.drop(2) else head.drop(1)) match {
              case Some(cliArg) =>
                if (cliArg.flag) {
                  rec(rest, appendMap(current, cliArg, ""))
                } else rest match{
                  case next :: rest2 => rec(rest2, appendMap(current, cliArg, next))
                  case Nil => Result.Error.MismatchedArguments(Nil, Nil, Nil, incomplete = Some(cliArg))
                }

              case None => complete(remaining, current)
            }
          }else if (allowPositional){

            keywordArgMap.values.find(as => !current.exists(_._1 == as)) match{
              case Some(nextInLine) => rec(rest, appendMap(current, nextInLine, head))
              case None => complete(remaining, current)
            }
          } else complete(remaining, current)

        case _ => complete(remaining, current)

      }
    }
    def complete(remaining: List[String],
                 current: Map[ArgSig[T], Vector[String]]): Result[Grouping[T]] = {

      val duplicates = current.filter(_._2.size > 1).toSeq
      val missing = argSigs.filter(x =>
        x.default.isEmpty && !current.contains(x) && !x.varargs && !x.flag
      )
      val unknown = if (argSigs.exists(_.varargs)) Nil else remaining
      if (missing.nonEmpty || duplicates.nonEmpty || unknown.nonEmpty){
        Result.Error.MismatchedArguments(
          missing = missing,
          unknown = unknown,
          duplicate = duplicates,
          incomplete = None
        )
      } else Result.Success(Grouping(remaining, current.map{case (k, Seq(v)) => (k, v)}))

    }
    rec(flatArgs, Map())
  }
}
