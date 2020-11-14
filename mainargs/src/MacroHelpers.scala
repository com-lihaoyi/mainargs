package mainargs

import scala.language.experimental.macros

object MacroHelpers{

  def readVarargs[T](arg: ArgSig[_],
                     values: Seq[String],
                     thunk: String => T) = {
    val attempts =
      for(item <- values)
      yield Util.tryEither(thunk(item), Result.ParamError.Invalid(arg, item, _))

    val bad = attempts.collect{ case Left(x) => x}
    if (bad.nonEmpty) Left(bad)
    else Right(attempts.collect{case Right(x) => x})
  }
  def read[T](dict: Map[String, String],
              default: => Option[Any],
              arg: ArgSig[_],
              thunk: String => T): Util.FailMaybe = {
    dict.get(arg.name) match{
      case None =>
        Util.tryEither(default.get, Result.ParamError.DefaultFailed(arg, _)).left.map(Seq(_))

      case Some(x) =>
        Util.tryEither(thunk(x), Result.ParamError.Invalid(arg, x, _)).left.map(Seq(_))
    }
  }


  def validate(args: Seq[Util.FailMaybe]): Result[Seq[Any]] = {
    val lefts = args.collect{case Left(x) => x}.flatten
    if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
    else Result.Success(args.collect{case Right(x) => x})
  }

  def makeReadCall[T: Read](dict: Map[String, String],
                            default: => Option[Any],
                            arg: ArgSig[_]) = {
    read[T](dict, default, arg, implicitly[Read[T]].read(None, _))
  }
  def makeReadVarargsCall[T: Read](arg: ArgSig[_],
                                   values: Seq[String]) = {
    readVarargs[T](arg, values, implicitly[Read[T]].read(None, _))
  }
}

