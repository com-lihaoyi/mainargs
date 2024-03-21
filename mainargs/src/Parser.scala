package mainargs

import acyclic.skipped

import scala.language.experimental.macros
import java.io.PrintStream
import scala.annotation.unroll
object ParserForMethods extends ParserForMethodsCompanionVersionSpecific
class ParserForMethods[B](val mains: MethodMains[B]) {
  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map(),
      @unroll sorted: Boolean = true,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): String = {
    Renderer.formatMainMethods(
      mains.value,
      totalWidth,
      docsOnNewLine,
      customNames,
      customDocs,
      sorted,
      nameMapper
    )
  }

  def runOrExit(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      stderr: PrintStream = System.err,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map()
  ): Any = {
    runEither(
      args,
      allowPositional,
      allowRepeats,
      totalWidth,
      printHelpOnExit,
      docsOnNewLine,
      autoPrintHelpAndExit,
      customNames,
      customDocs
    ) match {
      case Left(msg) =>
        stderr.println(msg)
        Compat.exit(1)
      case Right(v) => v
    }
  }

  def runOrThrow(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map(),
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Any = {
    runEither(
      args,
      allowPositional,
      allowRepeats,
      totalWidth,
      printHelpOnExit,
      docsOnNewLine,
      autoPrintHelpAndExit,
      customNames,
      customDocs,
      nameMapper = nameMapper
    ) match {
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }

  def runEither(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map(),
      @unroll sorted: Boolean = false,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Either[String, Any] = {
    if (autoPrintHelpAndExit.nonEmpty && args.take(1) == Seq("--help")) {
      val (exitCode, outputStream) = autoPrintHelpAndExit.get
      outputStream.println(helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted, nameMapper))
      Compat.exit(exitCode)
    } else runRaw0(args, allowPositional, allowRepeats, nameMapper) match {
      case Left(err) => Left(Renderer.renderEarlyError(err))
      case Right((main, res)) =>
        res match {
          case Result.Success(v) => Right(v)
          case f: Result.Failure =>
            Left(
              Renderer.renderResult(
                main,
                f,
                totalWidth,
                printHelpOnExit,
                docsOnNewLine,
                customNames.get(main.name(nameMapper)),
                customDocs.get(main.name(nameMapper)),
                sorted,
                nameMapper
              )
            )
        }
    }
  }

  def runRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Result[Any] = {
    runRaw0(args, allowPositional, allowRepeats, nameMapper) match {
      case Left(err) => err
      case Right((main, res)) => res
    }
  }

  def runRaw0(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Either[Result.Failure.Early, (MainData[_, B], Result[Any])] = {
    for (tuple <- Invoker.runMains(mains, args, allowPositional, allowRepeats, nameMapper)) yield {
      val (errMsg, res) = tuple
      (errMsg, res)
    }
  }
}

object ParserForClass extends ParserForClassCompanionVersionSpecific
class ParserForClass[T](val main: MainData[T, Any], val companion: () => Any)
    extends TokensReader.Class[T] {

  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customName: String = null,
      customDoc: String = null,
      @unroll sorted: Boolean = true,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): String = {
    Renderer.formatMainMethodSignature(
      main,
      0,
      totalWidth,
      Renderer.getLeftColWidth(main.renderedArgSigs, nameMapper),
      docsOnNewLine,
      Option(customName),
      Option(customDoc),
      sorted,
      nameMapper
    )
  }

  def constructOrExit(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      stderr: PrintStream = System.err,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customName: String = null,
      customDoc: String = null,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): T = {
    constructEither(
      args,
      allowPositional,
      allowRepeats,
      totalWidth,
      printHelpOnExit,
      docsOnNewLine,
      autoPrintHelpAndExit,
      customName,
      customDoc,
      sorted = false,
      nameMapper
    ) match {
      case Left(msg) =>
        stderr.println(msg)
        Compat.exit(1)
      case Right(v) => v
    }
  }

  def constructOrThrow(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customName: String = null,
      customDoc: String = null,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): T = {
    constructEither(
      args,
      allowPositional,
      allowRepeats,
      totalWidth,
      printHelpOnExit,
      docsOnNewLine,
      autoPrintHelpAndExit,
      customName,
      customDoc,
      sorted = false,
      nameMapper
    ) match {
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }

  def constructEither(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      totalWidth: Int = 100,
      printHelpOnExit: Boolean = true,
      docsOnNewLine: Boolean = false,
      autoPrintHelpAndExit: Option[(Int, PrintStream)] = Some((0, System.out)),
      customName: String = null,
      customDoc: String = null,
      @unroll sorted: Boolean = true,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Either[String, T] = {
    if (autoPrintHelpAndExit.nonEmpty && args.take(1) == Seq("--help")) {
      val (exitCode, outputStream) = autoPrintHelpAndExit.get
      outputStream.println(helpText(totalWidth, docsOnNewLine, customName, customDoc, sorted))
      Compat.exit(exitCode)
    } else constructRaw(args, allowPositional, allowRepeats, nameMapper) match {
      case Result.Success(v) => Right(v)
      case f: Result.Failure =>
        Left(
          Renderer.renderResult(
            main,
            f,
            totalWidth,
            printHelpOnExit,
            docsOnNewLine,
            Option(customName),
            Option(customDoc),
            sorted,
            nameMapper
          )
        )
    }
  }

  def constructRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      @unroll nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Result[T] = {
    Invoker.construct[T](this, args, allowPositional, allowRepeats, nameMapper)
  }
}
