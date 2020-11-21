package mainargs


object MainUtils {
  def construct[T](cep: ClassMains[T],
                   args: Seq[String],
                   allowPositional: Boolean) = {
    TokenGrouping.groupArgs(args, cep.main.argSigs, allowPositional)
      .flatMap(invoke(cep.companion(), cep.main, _))
      .map(_.value.asInstanceOf[T])
  }
  def invoke0[B](base: B,
                 mainData: MainData[B],
                 kvs: Map[String, String],
                 extras: Seq[String]): Result[Computed[Any]] = {
    val readArgValues: Seq[Either[Result[Computed[Any]], ParamResult]] = for(a <- mainData.argSigs0) yield {
      a match{
        case a: ArgSig[B] =>
          if (a.varargs) Right(makeReadVarargsCall(a, extras))
          else Right(makeReadCall(kvs, base, a))
        case a: ClassArgSig[_, B] =>
          Left(invoke0(a.reader.mains.companion(), a.reader.mains.main, kvs, extras))
      }
    }

    val validated = {
      val lefts = readArgValues
        .collect{
          case Left(Result.Error.InvalidArguments(lefts)) => lefts
          case Right(ParamResult.Failure(failure)) => failure
        }
        .flatten
      if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
      else Result.Success(
        readArgValues.collect{
          case Left(Result.Success(x)) => x
          case Right(ParamResult.Success(x)) => x
        }
      )
    }

    val res = validated.flatMap{ validated =>
      Result.Success(mainData.invokeRaw(base, validated))
    }
    res
  }
  def invoke[B](target: B,
                main: MainData[B],
                grouping: TokenGrouping[B]): Result[Computed[Any]] = {
    try invoke0(
      target,
      main,
      grouping.grouped.map{case (k, b) => (k.name, b)}, grouping.remaining
    ) catch{case e: Throwable => Result.Error.Exception(e)}
  }
  def runMains[B](mains: BasedMains[B],
                  args: Seq[String],
                  allowPositional: Boolean): Either[Result.Error.Early, (MainData[B], Result[Computed[Any]])] = {
    def groupArgs(main: MainData[B], argsList: Seq[String]) = Right(
      main,
      TokenGrouping.groupArgs(argsList, main.argSigs, allowPositional)
        .flatMap(MainUtils.invoke(mains.base(), main, _))
    )
    mains.value match{
      case Seq() => Left(Result.Error.Early.NoMainMethodsDetected())
      case Seq(main) => groupArgs(main, args)
      case multiple =>
        args.toList match{
          case List() => Left(Result.Error.Early.SubcommandNotSpecified(multiple.map(_.name)))
          case head :: tail =>
            if (head.startsWith("-")) {
              Left(Result.Error.Early.SubcommandSelectionDashes(head))
            } else{
              multiple.find(_.name == head) match{
                case None => Left(Result.Error.Early.UnableToFindSubcommand(head))
                case Some(main) => groupArgs(main, tail)
              }
            }
        }
    }
  }

  def tryEither[T](t: => T,
                   error: Throwable => Result.ParamError): Either[Result.ParamError, T] = {
    try Right(t)
    catch{ case e: Throwable => Left(error(e))}
  }
  def makeReadCall[B](dict: Map[String, String],
                      base: B,
                      arg: ArgSig[B]): ParamResult = {
    def default = arg.default.map(f => Computed(f(base)))
    dict.get(arg.name) match{
      case None =>
        tryEither(Right(default.get), Result.ParamError.DefaultFailed(arg, _)) match{
          case Left(ex) => ParamResult.Failure(Seq(ex))
          case Right(Right(v)) => ParamResult.Success(v)
        }

      case Some(token) =>
        if (arg.flag) ParamResult.Success(Computed(true))
        else tryEither(arg.reader.read(None, token), Result.ParamError.Exception(arg, token, _)) match{
          case Left(ex) => ParamResult.Failure(Seq(ex))
          case Right(Left(errMsg)) => ParamResult.Failure(Seq(Result.ParamError.Failed(arg, token, errMsg)))
          case Right(Right(v)) => ParamResult.Success(Computed(v))
        }
    }
  }

  def makeReadVarargsCall[B](arg: ArgSig[B],
                             values: Seq[String]): ParamResult = {
    val attempts =
      for(token <- values)
        yield tryEither(arg.reader.read(None, token), Result.ParamError.Exception(arg, token, _)) match{
          case Left(x) => Left(x)
          case Right(Left(errMsg)) => Left(Result.ParamError.Failed(arg, token, errMsg))
          case Right(Right(v)) => Right(v)
        }

    attempts.collect{ case Left(x) => x} match{
      case Nil => ParamResult.Success(Computed(attempts.collect{case Right(x) => x}))
      case bad => ParamResult.Failure(bad)
    }
  }
}
