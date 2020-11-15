package mainargs

import scala.language.experimental.macros

object MacroHelpers{
  def readVarargs[T](arg: ArgSig[_],
                     values: Seq[String],
                     thunk: String => Either[String, T]): Either[Seq[Result.ParamError], Seq[T]] = {
    val attempts =
      for(token <- values)
      yield Util.tryEither(thunk(token), Result.ParamError.Exception(arg, token, _)) match{
        case Left(x) => Left(x)
        case Right(Left(errMsg)) => Left(Result.ParamError.Failed(arg, token, errMsg))
        case Right(Right(v)) => Right(v)
      }

    attempts.collect{ case Left(x) => x} match{
      case Nil => Right(attempts.collect{case Right(x) => x})
      case bad => Left(bad)
    }
  }
  def readSingleArg[T](dict: Map[String, String],
                       default: => Option[Computed[T]],
                       arg: ArgSig[_],
                       thunk: String => Either[String, Computed[T]]): Util.FailMaybe = {
    dict.get(arg.name) match{
      case None =>
        Util.tryEither(Right(default.get), Result.ParamError.DefaultFailed(arg, _)) match{
          case Left(ex) => Left(Seq(ex))
          case Right(Right(v)) => Right(v)
        }

      case Some(token) =>
        if (arg.flag) Right(Computed(true))
        else Util.tryEither(thunk(token), Result.ParamError.Exception(arg, token, _)) match{
          case Left(ex) => Left(Seq(ex))
          case Right(Left(errMsg)) => Left(Seq(Result.ParamError.Failed(arg, token, errMsg)))
          case Right(Right(v)) => Right(v)
        }
    }
  }

  def validate(args: Seq[Util.FailMaybe]): Result[Seq[Computed[Any]]] = {
    val lefts = args.collect{case Left(x) => x}.flatten
    if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
    else Result.Success(args.collect{case Right(x) => x})
  }

  def getShortName[T: ArgParser] = implicitly[ArgParser[T]].shortName
  def makeReadCall[T: ArgParser](dict: Map[String, String],
                                 default: => Option[T],
                                 arg: ArgSig[_]): Util.FailMaybe = {
    readSingleArg[T](
      dict, default.map(Computed(_)), arg,
      implicitly[ArgParser[T]].read(None, _).map(Computed(_))
    )
  }
  def makeReadVarargsCall[T: ArgParser](arg: ArgSig[_],
                                        values: Seq[String]): Util.FailMaybe = {
    readVarargs[T](arg, values, implicitly[ArgParser[T]].read(None, _)).map(Computed(_))
  }
}
