package mainargs

object Invoker {
  def construct[T](
      cep: TokensReader.Class[T],
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean
  ): Result[T] = construct(cep, args, allowPositional, allowRepeats, Util.nullNameMapper)

  def construct[T](
      cep: TokensReader.Class[T],
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      nameMapper: String => Option[String]
  ): Result[T] = {
    TokenGrouping
      .groupArgs(
        args,
        cep.main.flattenedArgSigs,
        allowPositional,
        allowRepeats,
        cep.main.argSigs0.exists(_.reader.isLeftover),
        nameMapper
      )
      .flatMap((group: TokenGrouping[Any]) => invoke(cep.companion(), cep.main, group))
  }

  def invoke0[T, B](
      base: B,
      mainData: MainData[T, B],
      kvs: Map[ArgSig, Seq[String]],
      extras: Seq[String]
  ): Result[T] = {
    val readArgValues: Seq[Either[Result[Any], ParamResult[_]]] =
      for (a <- mainData.argSigs0) yield {
        a.reader match {
          case r: TokensReader.Flag =>
            Right(ParamResult.Success(Flag(kvs.contains(a)).asInstanceOf[T]))
          case r: TokensReader.Simple[T] => Right(makeReadCall(kvs, base, a, r))
          case r: TokensReader.Constant[T] => Right(r.read() match {
              case Left(s) => ParamResult.Failure(Seq(Result.ParamError.Failed(a, Nil, s)))
              case Right(v) => ParamResult.Success(v)
            })
          case r: TokensReader.Leftover[T, _] => Right(makeReadVarargsCall(a, extras, r))
          case r: TokensReader.Class[T] =>
            Left(
              invoke0[T, B](
                r.companion().asInstanceOf[B],
                r.main.asInstanceOf[MainData[T, B]],
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
    runMains(mains, args, allowPositional, allowRepeats, Util.nullNameMapper)
  }
  def runMains[B](
      mains: MethodMains[B],
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      nameMapper: String => Option[String]
  ): Either[Result.Failure.Early, (MainData[Any, B], Result[Any])] = {
    def groupArgs(main: MainData[Any, B], argsList: Seq[String]) = {
      def invokeLocal(group: TokenGrouping[Any]) =
        invoke(mains.base(), main.asInstanceOf[MainData[Any, Any]], group)
      Right(
        main,
        TokenGrouping
          .groupArgs(
            argsList,
            main.flattenedArgSigs,
            allowPositional,
            allowRepeats,
            main.argSigs0.exists {
              case x: ArgSig => x.reader.isLeftover
              case _ => false
            },
            nameMapper
          )
          .flatMap(invokeLocal)
      )
    }
    mains.value match {
      case Seq() => Left(Result.Failure.Early.NoMainMethodsDetected())
      case Seq(main) => groupArgs(main, args)
      case multiple =>
        args.toList match {
          case List() =>
            Left(Result.Failure.Early.SubcommandNotSpecified(multiple.map(_.name(nameMapper))))
          case head :: tail =>
            if (head.startsWith("-")) {
              Left(Result.Failure.Early.SubcommandSelectionDashes(head))
            } else {
              multiple.find { m =>
                val name = m.name(nameMapper)
                name == head || (m.mainName.isEmpty && m.defaultName == head)
              } match {
                case None => Left(Result.Failure.Early.UnableToFindSubcommand(
                    multiple.map(_.name(nameMapper)),
                    head
                  ))
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
  def makeReadCall[T](
      dict: Map[ArgSig, Seq[String]],
      base: Any,
      arg: ArgSig,
      reader: TokensReader.Simple[_]
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
    optResult.map(_.get.asInstanceOf[T])
  }

  def makeReadVarargsCall[T](
      arg: ArgSig,
      values: Seq[String],
      reader: TokensReader.Leftover[_, _]
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

    eithers match {
      case Left(s) => ParamResult.Failure(Seq(s))
      case Right(v) => ParamResult.Success(v.asInstanceOf[T])
    }
  }
}
