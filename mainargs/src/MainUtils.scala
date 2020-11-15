package mainargs
object MainUtils {
  def runMains[T](mains: Mains[T],
                  args: Seq[String],
                  allowPositional: Boolean,
                  totalWidth: Int): Either[Result.Error.Early, (Main[T], Result[Computed[Any]])] = {

    mains.value match{
      case Seq() => Left(Result.Error.Early.NoMainMethodsDetected())
      case Seq(main) =>
        Right(
          main,
          Grouping.groupArgs(args.toList, main.argSigs, allowPositional)
            .flatMap(main.invoke(mains.base(), _))
        )

      case multiple =>
        args.toList match{
          case List() => Left(Result.Error.Early.SubcommandNotSpecified(multiple.map(_.name)))
          case head :: tail =>
            if (head.startsWith("-")) {
              Left(Result.Error.Early.SubcommandSelectionDashes(head))
            } else{
              multiple.find(_.name == head) match{
                case None => Left(Result.Error.Early.UnableToFindSubcommand(head))
                case Some(main) =>
                  Right(
                    main,
                    Grouping
                      .groupArgs(tail, main.argSigs, allowPositional)
                      .flatMap(main.invoke(mains.base(), _))
                  )

              }
            }
        }
    }
  }
}
