package mainargs

import scala.language.experimental.macros

object MacroHelpers{

  def validate(args: Seq[Util.FailMaybe]): Result[Seq[Computed[Any]]] = {
    val lefts = args.collect{case Left(x) => x}.flatten
    if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
    else Result.Success(args.collect{case Right(x) => x})
  }

  def makeReadCall[B](dict: Map[String, String],
                      base: B,
                      arg: ArgSig[B]): Util.FailMaybe = {
    def default = arg.default.map(f => Computed(f(base)))
    dict.get(arg.name) match{
      case None =>
        Util.tryEither(Right(default.get), Result.ParamError.DefaultFailed(arg, _)) match{
          case Left(ex) => Left(Seq(ex))
          case Right(Right(v)) => Right(v)
        }

      case Some(token) =>
        if (arg.flag) Right(Computed(true))
        else Util.tryEither(arg.reader.read(None, token), Result.ParamError.Exception(arg, token, _)) match{
          case Left(ex) => Left(Seq(ex))
          case Right(Left(errMsg)) => Left(Seq(Result.ParamError.Failed(arg, token, errMsg)))
          case Right(Right(v)) => Right(Computed(v))
        }
    }
  }

  def makeReadVarargsCall[B](arg: ArgSig[B],
                             values: Seq[String]): Util.FailMaybe = {
    val attempts =
      for(token <- values)
        yield Util.tryEither(arg.reader.read(None, token), Result.ParamError.Exception(arg, token, _)) match{
          case Left(x) => Left(x)
          case Right(Left(errMsg)) => Left(Result.ParamError.Failed(arg, token, errMsg))
          case Right(Right(v)) => Right(v)
        }

    attempts.collect{ case Left(x) => x} match{
      case Nil => Right(Computed(attempts.collect{case Right(x) => x}))
      case bad => Left(bad)
    }
  }
}
