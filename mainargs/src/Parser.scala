package mainargs

import java.io.PrintStream

case class Parser(args: Array[String],
                  allowPositional: Boolean,
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

    Renderer.renderResult(
      cep.companion,
      cep.main,
      Grouping.groupArgs(args, cep.main.argSigs, allowPositional)
        .flatMap(cep.main.invoke(cep.companion, _))
        .map(_.value.asInstanceOf[T]),
      totalWidth
    )
  }

  def runOrExit[T: EntryPoints]: Unit = runEither[T] match{
    case Left(msg) =>
      stderr.println(msg)
      sys.exit(1)
    case Right(v) => v
  }
  def runOrThrow[T: EntryPoints]: Unit = runEither[T] match{
    case Left(msg) => throw new Exception(msg)
    case Right(v) => v
  }
  def runEither[T: EntryPoints]: Either[String, Unit] = {
    val eps = implicitly[EntryPoints[T]]
    Main.runMains(eps, args, allowPositional, totalWidth).map(_ => ())

  }

}