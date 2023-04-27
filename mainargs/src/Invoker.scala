package mainargs

object Invoker {
  def construct[T](
      cep: ClassMains[T],
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean
  ): Result[T] = {
    TokenGrouping
      .groupArgs(
        args,
        cep.main.argSigs,
        allowPositional,
        allowRepeats,
        cep.main.argSigs0.exists {
          case x: ArgSig.Simple[_, _] => x.reader.isLeftover
          case _ => false
        }
      )
      .flatMap(invoke(cep.companion(), cep.main, _))
  }

  def invoke0[T, B](
      base: B,
      mainData: MainData[T, B],
      kvs: Map[ArgSig.Named[_, B], Seq[String]],
      extras: Seq[String]
  ): Result[T] = {
    val readArgValues: Seq[Either[Result[Any], ParamResult[_]]] =
      for (a <- mainData.argSigs0) yield {
        a match {
          case a: ArgSig.Flag[B] =>
            Right(ParamResult.Success(Flag(kvs.contains(a)).asInstanceOf[T]))

          case a: ArgSig.Simple[T, B] =>
            a.reader match{
              case r: TokensReader.Simple[T] => Right(makeReadCall(kvs, base, a, r))
              case r: TokensReader.Leftover[T, _] => Right(makeReadVarargsCall(a, extras, r))
            }

          case a: ArgSig.Class[T, B] =>
            Left(
              invoke0[T, B](
                a.reader.companion().asInstanceOf[B],
                a.reader.main.asInstanceOf[MainData[T, B]],
                kvs,
                extras
              )
            )
        }
      }

    val validated = {
      val lefts = readArgValues
        .collect {
          case Left(Result.Failure.InvalidArguments(lefts)) => lefts
          case Right(ParamResult.Failure(failure)) => failure
        }
        .flatten
      if (lefts.nonEmpty) Result.Failure.InvalidArguments(lefts)
      else Result.Success(
        readArgValues.collect {
          case Left(Result.Success(x)) => x
          case Right(ParamResult.Success(x)) => x
        }
      )
    }

    val res = validated.flatMap { validated =>
      Result.Success(mainData.invokeRaw(base, validated))
    }
    res
  }
  def invoke[T, B](target: B, main: MainData[T, B], grouping: TokenGrouping[B]): Result[T] = {
    try invoke0(
        target,
        main,
        grouping.grouped,
        grouping.remaining
      )
    catch { case e: Throwable => Result.Failure.Exception(e) }
  }
  def runMains[B](
      mains: MethodMains[B],
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean
  ): Either[Result.Failure.Early, (MainData[Any, B], Result[Any])] = {
    def groupArgs(main: MainData[Any, B], argsList: Seq[String]) = Right(
      main,
      TokenGrouping
        .groupArgs(
          argsList,
          main.argSigs,
          allowPositional,
          allowRepeats,
          main.argSigs0.exists {
            case x: ArgSig.Simple[_, _] => x.reader.isLeftover
            case _ => false
          }
        )
        .flatMap(Invoker.invoke(mains.base(), main, _))
    )
    mains.value match {
      case Seq() => Left(Result.Failure.Early.NoMainMethodsDetected())
      case Seq(main) => groupArgs(main, args)
      case multiple =>
        args.toList match {
          case List() => Left(Result.Failure.Early.SubcommandNotSpecified(multiple.map(_.name)))
          case head :: tail =>
            if (head.startsWith("-")) {
              Left(Result.Failure.Early.SubcommandSelectionDashes(head))
            } else {
              multiple.find(_.name == head) match {
                case None =>
                  Left(Result.Failure.Early.UnableToFindSubcommand(multiple.map(_.name), head))
                case Some(main) => groupArgs(main, tail)
              }
            }
        }
    }
  }

  def tryEither[T](t: => T, error: Throwable => Result.ParamError): Either[Result.ParamError, T] = {
    try Right(t)
    catch { case e: Throwable => Left(error(e)) }
  }
  def makeReadCall[T, B](
      dict: Map[ArgSig.Named[_, B], Seq[String]],
      base: B,
      arg: ArgSig.Simple[T, B],
      reader: TokensReader.Simple[T]
  ): ParamResult[T] = {
    def prioritizedDefault = tryEither(
      arg.default.map(_(base)),
      Result.ParamError.DefaultFailed(arg, _)
    ) match {
      case Left(ex) => ParamResult.Failure(Seq(ex))
      case Right(v) => ParamResult.Success(v)
    }
    val tokens = dict.get(arg) match {
      case None => if (reader.allowEmpty) Some(Nil) else None
      case Some(tokens) => Some(tokens)
    }
    val optResult = tokens match {
      case None => prioritizedDefault
      case Some(tokens) =>
        tryEither(
          reader.read(tokens),
          Result.ParamError.Exception(arg, tokens, _)
        ) match {
          case Left(ex) => ParamResult.Failure(Seq(ex))
          case Right(Left(errMsg)) =>
            ParamResult.Failure(Seq(Result.ParamError.Failed(arg, tokens, errMsg)))
          case Right(Right(v)) => ParamResult.Success(Some(v))
        }
    }
    optResult.map(_.get)
  }

  def makeReadVarargsCall[T, B](
      arg: ArgSig.Simple[T, B],
      values: Seq[String],
      reader: TokensReader.Leftover[T, _]
  ): ParamResult[T] = {
    val eithers =
      tryEither(
        reader.read(values),
        Result.ParamError.Exception(arg, values, _)
      ) match {
        case Left(x) => Left(x)
        case Right(Left(errMsg)) => Left(Result.ParamError.Failed(arg, values, errMsg))
        case Right(Right(v)) => Right(v)
      }

    eithers match{
      case Left(s) => ParamResult.Failure(Seq(s))
      case Right(v) => ParamResult.Success(v)
    }
  }
}
