package mainargs
object Main {
  def runMains[T](target: T, mains: EntryPoints[T], args: Array[String]): Either[String, Any] = {


    mains.value match{
      case Seq() => Left("No @main methods declared")
      case Seq(main) =>
        for(groupedArgs <- Grouping.groupArgs(args.toList, main.argSigs))
        yield Renderer.renderResult(target, main, main.invoke(target, groupedArgs))
      case multiple =>
        lazy val suffix = Renderer.formatMainMethods(target, multiple)
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
                  for(groupedArgs <- Grouping.groupArgs(tail.toList, main.argSigs))
                  yield Renderer.renderResult(target, main, main.invoke(target, groupedArgs))
              }
            }
        }
    }
  }
}
