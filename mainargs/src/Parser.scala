package mainargs
import acyclic.skipped
import scala.language.experimental.macros
import java.io.PrintStream
object ParserForMethods{
  def apply[B](base: B): ParserForMethods[B] = macro Macros.parserForMethods[B]
}
class ParserForMethods[B](val mains: BasedMains[B]){
  def helpText(totalWidth: Int = 95) = {
    Renderer.formatMainMethods(mains.value, totalWidth)
  }
  def runOrExit(args: Seq[String],
                allowPositional: Boolean = true,
                stderr: PrintStream = System.err,
                totalWidth: Int = 95): Any = {
    runEither(args, allowPositional, totalWidth) match{
      case Left(msg) =>
        stderr.println(msg)
        sys.exit(1)
      case Right(v) => v
    }
  }
  def runOrThrow(args: Seq[String],
                 allowPositional: Boolean = true): Any = {
    runEither(args, allowPositional) match{
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }
  def runEither(args: Seq[String],
                allowPositional: Boolean = true,
                totalWidth: Int = 95): Either[String, Any] = {
    runRaw0(args, allowPositional) match {
      case Left(err) => Left(Renderer.renderEarlyError(err))
      case Right((main, res)) =>
        Renderer.renderResult(mains.base, main, res, totalWidth)
    }
  }
  def runRaw(args: Seq[String], allowPositional: Boolean = true): Result[Any] = {
    runRaw0(args, allowPositional) match{
      case Left(err) => err
      case Right((main, res)) => res
    }
  }

  def runRaw0(args: Seq[String], allowPositional: Boolean = true): Either[Result.Error.Early, (MainData[B], Result[Any])] = {
    for (tuple <- MainUtils.runMains(mains, args, allowPositional)) yield {
      val (errMsg, res) = tuple
      (errMsg, res.map(_.value))
    }
  }
}

object ParserForClass{
  def apply[T]: ParserForClass[T] = macro Macros.parserForClass[T]
}
class ParserForClass[T](val mains: ClassMains[T]){
  def helpText(totalWidth: Int = 95) = {
    Renderer.formatMainMethodSignature(mains.main, 0, totalWidth)
  }
  def constructOrExit(args: Seq[String],
                      allowPositional: Boolean = true,
                      stderr: PrintStream = System.err,
                      totalWidth: Int = 95): T = {
    constructEither(args, allowPositional, totalWidth) match{
      case Left(msg) =>
        stderr.println(msg)
        sys.exit(1)
      case Right(v) => v
    }
  }
  def constructOrThrow(args: Seq[String],
                       allowPositional: Boolean = true,
                       totalWidth: Int = 95): T = {
    constructEither(args, allowPositional, totalWidth) match{
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }
  def constructEither(args: Seq[String],
                      allowPositional: Boolean = true,
                      totalWidth: Int = 95): Either[String, T] = {

    Renderer.renderResult[Any, T](
      mains.companion,
      mains.main,
      constructRaw(args, allowPositional),
      totalWidth
    )
  }
  def constructRaw(args: Seq[String],
                   allowPositional: Boolean = true): Result[T] = {
    MainUtils.construct[T](mains, args, allowPositional)
  }
}
