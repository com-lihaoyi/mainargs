package mainargs

object MainUtils {
  def construct[T](cep: ClassMains[T],
                   args: Seq[String],
                   allowPositional: Boolean,
                   allowRepeats: Boolean): Result[T] = {
    TokenGrouping
      .groupArgs(
        args,
        cep.main.argSigs,
        allowPositional,
        allowRepeats,
        cep.main.leftoverArgSig.nonEmpty
      )
      .flatMap(invoke(cep.companion(), cep.main, _))
  }
  def invoke0[T, B](base: B,
                    mainData: MainData[T, B],
                    kvs: Map[String, Seq[String]],
                    extras: Seq[String]): Result[T] = {
    val readArgValues: Seq[Either[Result[Any], ParamResult[_]]] = for(a <- mainData.argSigs0) yield {
      a match{
        case a: ArgSig.Simple[T, B] => Right(makeReadCall(kvs, base, a))
        case a: ArgSig.Leftover[T, B] =>
          Right(makeReadVarargsCall(a, extras).map(x => LeftoverTokens(x:_*).asInstanceOf[T]))
        case a: ArgSig.Class[T, B] =>
          Left(invoke0(a.reader.companion(), a.reader.main, kvs, extras))
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
  def invoke[T, B](target: B,
                   main: MainData[T, B],
                   grouping: TokenGrouping[B]): Result[T] = {
    try invoke0(
      target,
      main,
      grouping.grouped.map{case (k, vs) => (k.name, vs)},
      grouping.remaining
    ) catch{case e: Throwable => Result.Error.Exception(e)}
  }
  def runMains[B](mains: MethodMains[B],
                  args: Seq[String],
                  allowPositional: Boolean,
                  allowRepeats: Boolean): Either[Result.Error.Early, (MainData[Any, B], Result[Any])] = {
    def groupArgs(main: MainData[Any, B], argsList: Seq[String]) = Right(
      main,
      TokenGrouping
        .groupArgs(
          argsList,
          main.argSigs,
          allowPositional,
          allowRepeats,
          main.leftoverArgSig.nonEmpty
        )
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
  def makeReadCall[T, B](dict: Map[String, Seq[String]],
                         base: B,
                         arg: ArgSig.Simple[T, B]): ParamResult[T] = {
    if (arg.flag) ParamResult.Success(dict.contains(arg.name).asInstanceOf[T])
    else{
      def prioritizedDefault = tryEither(
        arg.default.map(_(base)),
        Result.ParamError.DefaultFailed(arg, _)
      ) match{
        case Left(ex) => ParamResult.Failure(Seq(ex))
        case Right(v) => ParamResult.Success(v)
      }
      val tokens = dict.get(arg.name) match{
        case None =>  if (arg.reader.allowEmpty) Some(Nil) else None
        case Some(tokens) => Some(tokens)
      }
      val optResult = tokens match{
        case None => prioritizedDefault
        case Some(tokens) =>
          tryEither(
            arg.reader.read(tokens),
            Result.ParamError.Exception(arg, tokens, _)
          ) match{
            case Left(ex) => ParamResult.Failure(Seq(ex))
            case Right(Left(errMsg)) => ParamResult.Failure(Seq(Result.ParamError.Failed(arg, tokens, errMsg)))
            case Right(Right(v)) => ParamResult.Success(Some(v))
          }
      }
      optResult.map(_.get)
    }
  }

  def makeReadVarargsCall[T, B](arg: ArgSig.Leftover[T, B],
                                values: Seq[String]): ParamResult[Seq[T]] = {
    val attempts =
      for(token <- values)
      yield tryEither(arg.reader.read(Seq(token)), Result.ParamError.Exception(arg, Seq(token), _)) match{
        case Left(x) => Left(x)
        case Right(Left(errMsg)) => Left(Result.ParamError.Failed(arg, Seq(token), errMsg))
        case Right(Right(v)) => Right(v)
      }

    attempts.collect{ case Left(x) => x} match{
      case Nil => ParamResult.Success(attempts.collect{case Right(x) => x})
      case bad => ParamResult.Failure(bad)
    }
  }
}
