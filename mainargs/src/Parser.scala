package mainargs

import java.io.PrintStream

case class Parser(args: Seq[String],
                  allowPositional: Boolean = true,
                  stderr: PrintStream = System.err,
                  totalWidth: Int = 95){

  def constructOrExit[T: ClassMains]: T = constructEither[T] match{
    case Left(msg) =>
      stderr.println(msg)
      sys.exit(1)
    case Right(v) => v
  }
  def constructOrThrow[T: ClassMains]: T = constructEither[T] match{
    case Left(msg) => throw new Exception(msg)
    case Right(v) => v
  }
  def constructEither[T: ClassMains]: Either[String, T] = {
    val cep = implicitly[ClassMains[T]]

    Renderer.renderResult[Any, T](
      cep.companion,
      cep.main,
      constructRaw[T],
      totalWidth
    )
  }
  def constructRaw[T: ClassMains]: Result[T] = {
    MainUtils.construct[T](implicitly[ClassMains[T]], args, allowPositional)
  }

  def runOrExit[B: Mains]: Any = runEither[B] match{
    case Left(msg) =>
      stderr.println(msg)
      sys.exit(1)
    case Right(v) => v
  }
  def runOrThrow[B: Mains]: Any = runEither[B] match{
    case Left(msg) => throw new Exception(msg)
    case Right(v) => v
  }
  def runEither[B: Mains]: Either[String, Any] = {
    runRaw0[B] match {
      case Left(err) => Left(Renderer.renderEarlyError(err))
      case Right((main, res)) =>
        Renderer.renderResult(
          implicitly[Mains[B]].base, main, res, totalWidth
        )
    }
  }
  def runRaw[B: Mains]: Result[Any] = runRaw0[B] match{
    case Left(err) => err
    case Right((main, res)) => res
  }

  def runRaw0[B: Mains]: Either[Result.Error.Early, (MainData[B], Result[Any])] = {
    for (tuple <- MainUtils.runMains(implicitly[Mains[B]], args, allowPositional)) yield {
      val (errMsg, res) = tuple
      (errMsg, res.map(_.value))
    }
  }

  def runOrExit[B: BareMains](b: B): Any = {
    runOrExit[B](Mains(implicitly[BareMains[B]].value, () => b))
  }
  def runOrThrow[B: BareMains](b: B): Any = {
    runOrThrow[B](Mains(implicitly[BareMains[B]].value, () => b))
  }
  def runEither[B: BareMains](b: B): Either[String, Any] = {
    runEither[B](Mains(implicitly[BareMains[B]].value, () => b))
  }
  def runRaw[B: BareMains](b: B): Result[Any] = {
    runRaw[B](Mains(implicitly[BareMains[B]].value, () => b))
  }
  def runRaw0[B: BareMains](b: B): Either[Result.Error.Early, (MainData[B], Result[Any])] = {
    runRaw0[B](Mains(implicitly[BareMains[B]].value, () => b))
  }
}