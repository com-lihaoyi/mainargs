package mainargs
object Main {
  def runMains[T](mains: EntryPoints[T],
                  args: Seq[String],
                  allowPositional: Boolean,
                  totalWidth: Int): Either[Result.Error.Early, (EntryPoint[T], Result[Computed[Any]])] = {

    mains.value match{
      case Seq() => Left(Result.Error.Early.NoMainMethodsDetected())
      case Seq(main) =>
        Right(
          main,
          Grouping.groupArgs(args.toList, main.argSigs, allowPositional)
            .flatMap(main.invoke(mains.target(), _))
        )

      case multiple =>
        lazy val suffix = Renderer.formatMainMethods(mains.target(), multiple, totalWidth)
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
                      .flatMap(main.invoke(mains.target(), _))
                  )

              }
            }
        }
    }
  }
}
