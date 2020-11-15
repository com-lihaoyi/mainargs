package mainargs

import java.io.PrintStream

case class Parser(args: Seq[String],
                  allowPositional: Boolean = true,
                  stderr: PrintStream = System.err,
                  totalWidth: Int = 95){

  def constructOrExit[T: ClassEntryPoint]: T = constructEither[T] match{
    case Left(msg) =>
      stderr.println(msg)
      sys.exit(1)
    case Right(v) => v
  }
  def constructOrThrow[T: ClassEntryPoint]: T = constructEither[T] match{
    case Left(msg) => throw new Exception(msg)
    case Right(v) => v
  }
  def constructEither[T: ClassEntryPoint]: Either[String, T] = {
    val cep = implicitly[ClassEntryPoint[T]]

    Renderer.renderResult[Any, T](
      cep.companion,
      cep.main,
      constructRaw[T],
      totalWidth
    )
  }
  def constructRaw[T: ClassEntryPoint]: Result[T] = {
    val cep = implicitly[ClassEntryPoint[T]]
    Grouping.groupArgs(args, cep.main.argSigs, allowPositional)
      .flatMap(cep.main.invoke(cep.companion(), _))
      .map(_.value.asInstanceOf[T])
  }

  def runOrExit[B: EntryPoints]: Any = runEither[B] match{
    case Left(msg) =>
      stderr.println(msg)
      sys.exit(1)
    case Right(v) => v
  }
  def runOrThrow[B: EntryPoints]: Any = runEither[B] match{
    case Left(msg) => throw new Exception(msg)
    case Right(v) => v
  }
  def runEither[B: EntryPoints]: Either[String, Any] = {
    runRaw0[B] match {
      case Left(err) => Left(Renderer.renderEarlyError(err))
      case Right((main, res)) =>
        Renderer.renderResult(
          implicitly[EntryPoints[B]].target, main, res, totalWidth
        )
    }
  }
  def runRaw[B: EntryPoints]: Result[Any] = runRaw0[B] match{
    case Left(err) => err
    case Right((main, res)) => res
  }

  def runRaw0[B: EntryPoints]: Either[Result.Error.Early, (EntryPoint[B], Result[Any])] = {
    for {
      tuple <- Main.runMains(implicitly[EntryPoints[B]], args, allowPositional, totalWidth)
    } yield {
      val (errMsg, res) = tuple
      (errMsg, res.map(_.value))
    }
  }
}