package mainargs

import scala.collection.mutable


/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSignatures]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class EntryPoint[T](name: String,
                         argSignatures: Seq[ArgSig[T]],
                         doc: Option[String],
                         varargs: Boolean,
                         invoke0: (T, Map[String, String], Seq[String]) => Result[Any]){
  def invoke(target: T, groupedArgs: Seq[(String, Option[String])]): Result[Any] = {
    var remainingArgSignatures = argSignatures.toList


    val accumulatedKeywords = mutable.Map.empty[ArgSig[T], mutable.Buffer[String]]
    val keywordableArgs = if (varargs) argSignatures.dropRight(1) else argSignatures

    for(arg <- keywordableArgs) accumulatedKeywords(arg) = mutable.Buffer.empty

    val leftoverArgs = mutable.Buffer.empty[String]

    val lookupArgSig = argSignatures.map(x => (x.name, x)).toMap

    var incomplete: Option[ArgSig[T]] = None

    for(group <- groupedArgs){

      group match{
        case (value, None) =>
          if (value.startsWith("-") && !varargs){
            lookupArgSig.get(Util.stripDashes(value)) match{
              case None => leftoverArgs.append(value)
              case Some(sig) => incomplete = Some(sig)
            }

          } else remainingArgSignatures match {
            case Nil => leftoverArgs.append(value)
            case last :: Nil if varargs => leftoverArgs.append(value)
            case next :: rest =>
              accumulatedKeywords(next).append(value)
              remainingArgSignatures = rest
          }
        case (rawKey, Some(value)) =>
          val key = Util.stripDashes(rawKey)
          lookupArgSig.get(key) match{
            case Some(x) if accumulatedKeywords.contains(x) =>
              if (accumulatedKeywords(x).nonEmpty && varargs){
                leftoverArgs.append(rawKey, value)
              }else{
                accumulatedKeywords(x).append(value)
                remainingArgSignatures = remainingArgSignatures.filter(_.name != key)
              }
            case _ =>
              leftoverArgs.append(rawKey, value)
          }
      }
    }

    val missing0 = remainingArgSignatures.filter(_.default.isEmpty)
    val missing = if(varargs) {
      missing0.filter(_ != argSignatures.last)
    } else {
      missing0.filter(x => incomplete != Some(x))
    }
    val duplicates = accumulatedKeywords.toSeq.collect {
      case (k, l) if l.lengthCompare(1) > 0 =>
        k -> l.toSeq
    }

    if (
      incomplete.nonEmpty ||
        missing.nonEmpty ||
        duplicates.nonEmpty ||
        (leftoverArgs.nonEmpty && !varargs)
    ){
      Result.Error.MismatchedArguments(
        missing = missing,
        unknown = leftoverArgs.toSeq,
        duplicate = duplicates,
        incomplete = incomplete

      )
    } else {
      val mapping = accumulatedKeywords
        .iterator
        .collect{case (k, b) if b.lengthCompare(1) == 0 => (k.name, b.head)}
        .toMap

      try invoke0(target, mapping, leftoverArgs.toSeq)
      catch{case e: Throwable =>
        Result.Error.Exception(e)
      }
    }
  }
}

