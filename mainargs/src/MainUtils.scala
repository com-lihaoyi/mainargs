package mainargs
object MainUtils {
  def construct[T](cep: ClassMains[T],
                   args: Seq[String],
                   allowPositional: Boolean) = {
    TokenGrouping.groupArgs(args, cep.main.argSigs, allowPositional)
      .flatMap(invoke(cep.companion(), cep.main, _))
      .map(_.value.asInstanceOf[T])
  }
  def invoke[B](target: B,
                main: MainData[B],
                grouping: TokenGrouping[B]): Result[Computed[Any]] = {
    try main.invoke0(
      target,
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
}
