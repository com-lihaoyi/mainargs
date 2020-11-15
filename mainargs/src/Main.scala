package mainargs
object Main {
  def runMains[T](mains: EntryPoints[T],
                  args: Array[String],
                  allowPositional: Boolean,
                  totalWidth: Int): Either[String, (EntryPoint[T], Result[Computed[Any]])] = {

    mains.value match{
      case Seq() => Left("No @main methods declared")
      case Seq(main) =>
        Right(
          main,
          Grouping.groupArgs(args.toList, main.argSigs, allowPositional)
            .flatMap(main.invoke(mains.target(), _))
        )

      case multiple =>
        lazy val suffix = Renderer.formatMainMethods(mains.target(), multiple, totalWidth)
        args.toList match{
          case List() => Left("Need to specify a sub command: " + multiple.map(_.name).mkString(", "))
          case head :: tail =>
            if (head.startsWith("-")) {
              Left(
                "To select a subcommand to run, you don't need --s." + Renderer.newLine +
                s"Did you mean `${head.drop(2)}` instead of `$head`?"
              )
            } else{
              multiple.find(_.name == head) match{
                case None => Left(s"Unable to find subcommand: " + head + suffix)
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
