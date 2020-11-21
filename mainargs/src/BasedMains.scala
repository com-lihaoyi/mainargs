package mainargs
import acyclic.skipped
import mainargs.Util.FailMaybe

import scala.language.experimental.macros

case class BasedMains[B](value: Seq[MainData[B]], base: () => B)

case class ClassMains[T](main: MainData[Any], companion: () => Any)

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[MainUtils.invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class MainData[B](name: String,
                       argSigs0: Seq[AnyArgSig[B]],
                       doc: Option[String],
                       invokeRaw: (B, Seq[Computed[Any]]) => Computed[Any]){
  def invoke0(base: B, kvs: Map[String, String], extras: Seq[String]): Result[Computed[Any]] = {
    val readArgValues: Seq[Either[Result[Computed[Any]], FailMaybe]] = for(a <- argSigs0) yield {
      a match{
        case a: ArgSig[B] =>
          if (a.varargs) Right(MacroHelpers.makeReadVarargsCall(a, extras))
          else Right(MacroHelpers.makeReadCall(kvs, base, a))
        case a: ClassArgSig[_, B] =>
          Left(a.reader.mains.main.invoke0(a.reader.mains.companion(), kvs, extras))
      }
    }

    val validated = {
      val lefts = readArgValues
        .collect{
          case Left(Result.Error.InvalidArguments(lefts)) => lefts
          case Right(Left(failure)) => failure
        }
        .flatten
      if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
      else Result.Success(
        readArgValues.collect{
          case Left(Result.Success(x)) => x
          case Right(Right(x)) => x
        }
      )
    }

    val res = validated.flatMap{ validated =>
      Result.Success(invokeRaw(base, validated))
    }
    res
  }
  val argSigs = argSigs0.iterator.flatMap(AnyArgSig.flatten).toVector
  val varargs = argSigs.exists(_.varargs)
}
