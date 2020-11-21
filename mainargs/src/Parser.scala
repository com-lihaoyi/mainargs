package mainargs
import scala.language.experimental.macros
import java.io.PrintStream
object ParserForMethods{
  def apply[B](base: B): ParserForMethods[B] = macro Macros.parserForMethods[B]
}
class ParserForMethods[B](val mains: BasedMains[B]){
  def helpText(totalWidth: Int = 95, docsOnNewLine: Boolean = false) = {
    Renderer.formatMainMethods(mains.value, totalWidth, docsOnNewLine)
  }
  def runOrExit(args: Seq[String],
                allowPositional: Boolean = false,
                allowRepeats: Boolean = false,
                stderr: PrintStream = System.err,
                totalWidth: Int = 95,
                printHelpOnExit: Boolean = true,
                docsOnNewLine: Boolean = false): Any = {
    runEither(args, allowPositional, allowRepeats, totalWidth, printHelpOnExit, docsOnNewLine) match{
      case Left(msg) =>
        stderr.println(msg)
        sys.exit(1)
      case Right(v) => v
    }
  }
  def runOrThrow(args: Seq[String],
                 allowPositional: Boolean = false,
                 allowRepeats: Boolean = false,
                 totalWidth: Int = 95,
                 printHelpOnExit: Boolean = true,
                 docsOnNewLine: Boolean = false): Any = {
    runEither(args, allowPositional, allowRepeats, totalWidth, printHelpOnExit, docsOnNewLine) match{
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }
  def runEither(args: Seq[String],
                allowPositional: Boolean = false,
                allowRepeats: Boolean = false,
                totalWidth: Int = 95,
                printHelpOnExit: Boolean = true,
                docsOnNewLine: Boolean = false): Either[String, Any] = {
    runRaw0(args, allowPositional, allowRepeats) match {
      case Left(err) => Left(Renderer.renderEarlyError(err))
      case Right((main, res)) =>
        Renderer.renderResult(main, res, totalWidth, printHelpOnExit, docsOnNewLine)
    }
  }
  def runRaw(args: Seq[String],
             allowPositional: Boolean = false,
             allowRepeats: Boolean = false): Result[Any] = {
    runRaw0(args, allowPositional, allowRepeats) match{
      case Left(err) => err
      case Right((main, res)) => res
    }
  }

  def runRaw0(args: Seq[String],
              allowPositional: Boolean = false,
              allowRepeats: Boolean = false): Either[Result.Error.Early, (MainData[_, B], Result[Any])] = {
    for (tuple <- MainUtils.runMains(mains, args, allowPositional, allowRepeats)) yield {
      val (errMsg, res) = tuple
      (errMsg, res)
    }
  }
}

object ParserForClass{
  def apply[T]: ParserForClass[T] = macro Macros.parserForClass[T]
}
class ParserForClass[T](val mains: ClassMains[T]){
  def helpText(totalWidth: Int = 95, docsOnNewLine: Boolean = false) = {
    Renderer.formatMainMethodSignature(mains.main, 0, totalWidth, 0, docsOnNewLine)
  }
  def constructOrExit(args: Seq[String],
                      allowPositional: Boolean = false,
                      allowRepeats: Boolean = false,
                      stderr: PrintStream = System.err,
                      totalWidth: Int = 95,
                      printHelpOnExit: Boolean = true,
                      docsOnNewLine: Boolean = false): T = {
    constructEither(args, allowPositional, allowRepeats, totalWidth, printHelpOnExit, docsOnNewLine) match{
      case Left(msg) =>
        stderr.println(msg)
        sys.exit(1)
      case Right(v) => v
    }
  }
  def constructOrThrow(args: Seq[String],
                       allowPositional: Boolean = false,
                       allowRepeats: Boolean = false,
                       totalWidth: Int = 95,
                       printHelpOnExit: Boolean = true,
                       docsOnNewLine: Boolean = false): T = {
    constructEither(args, allowPositional, allowRepeats, totalWidth, printHelpOnExit, docsOnNewLine) match{
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }
  def constructEither(args: Seq[String],
                      allowPositional: Boolean = false,
                      allowRepeats: Boolean = false,
                      totalWidth: Int = 95,
                      printHelpOnExit: Boolean = true,
                      docsOnNewLine: Boolean = false): Either[String, T] = {

    Renderer.renderResult[Any, T](
      mains.main,
      constructRaw(args, allowPositional, allowRepeats),
      totalWidth,
      printHelpOnExit,
      docsOnNewLine
    )
  }
  def constructRaw(args: Seq[String],
                   allowPositional: Boolean = false,
                   allowRepeats: Boolean = false): Result[T] = {
    MainUtils.construct[T](mains, args, allowPositional, allowRepeats)
  }
}
