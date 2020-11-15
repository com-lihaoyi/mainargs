package mainargs

import scala.annotation.tailrec

case class EntryPoints[T](value: Seq[EntryPoint[T]], target: () => T)

case class ClassEntryPoint[T](main: EntryPoint[Any], companion: () => Any)

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class EntryPoint[T](name: String,
                         argSigs: Seq[ArgSig[T]],
                         doc: Option[String],
                         varargs: Boolean,
                         invoke0: (T, Map[String, String], Seq[String]) => Result[Computed[Any]]){
  def invoke(target: T, grouped: Grouping[T]): Result[Computed[Any]] = {
    try invoke0(
      target,
      grouped.grouped.map{case (k, b) => (k.name, b)}, grouped.remaining
    ) catch{case e: Throwable => Result.Error.Exception(e)}
  }
}


case class Grouping[T](remaining: List[String],
                       grouped: Map[ArgSig[T], String])
object Grouping{
  def groupArgs[T](flatArgs0: Seq[String],
                   argSigs: Seq[ArgSig[T]],
                   allowPositional: Boolean): Result[Grouping[T]] = {

    val flatArgs = flatArgs0.toList
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
