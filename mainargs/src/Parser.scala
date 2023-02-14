package mainargs

import acyclic.skipped

import scala.language.experimental.macros
import java.io.PrintStream

object ParserForMethods extends ParserForMethodsCompanionVersionSpecific
class ParserForMethods[B](val mains: MethodMains[B]) {
  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map(),
      sorted: Boolean = false
  ): String = {
    Renderer.formatMainMethods(
      mains.value,
      totalWidth,
      docsOnNewLine,
      customNames,
      customDocs,
      sorted
    )
  }

  /** binary compatibility shim. */
  private[mainargs] def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String]
  ): String = helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted = false)

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
      sorted: Boolean = false
  ): Either[String, Any] = {
    if (autoPrintHelpAndExit.nonEmpty && args.take(1) == Seq("--help")) {
      val (exitCode, outputStream) = autoPrintHelpAndExit.get
      outputStream.println(helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted))
      Compat.exit(exitCode)
    } else runRaw0(args, allowPositional, allowRepeats) match {
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
                customNames.get(main.name),
                customDocs.get(main.name),
                sorted
              )
            )
        }
    }
  }

  /** binary compatibility shim. */
  private[mainargs] def runEither(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customNames: Map[String, String],
      customDocs: Map[String, String]
  ): Either[String, Any] = runEither(
    args,
    allowPositional,
    allowRepeats,
    totalWidth,
    printHelpOnExit,
    docsOnNewLine,
    autoPrintHelpAndExit,
    customNames,
    customDocs,
    sorted = false
  )

  def runRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false
  ): Result[Any] = {
    runRaw0(args, allowPositional, allowRepeats) match {
      case Left(err) => err
      case Right((main, res)) => res
    }
  }

  def runRaw0(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false
  ): Either[Result.Failure.Early, (MainData[_, B], Result[Any])] = {
    for (tuple <- Invoker.runMains(mains, args, allowPositional, allowRepeats)) yield {
      val (errMsg, res) = tuple
      (errMsg, res)
    }
  }
}

object ParserForClass extends ParserForClassCompanionVersionSpecific
class ParserForClass[T](val mains: ClassMains[T]) extends SubParser[T] {
  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customName: String = null,
      customDoc: String = null,
      sorted: Boolean = false
  ): String = {
    Renderer.formatMainMethodSignature(
      mains.main,
      0,
      totalWidth,
      Renderer.getLeftColWidth(mains.main.argSigs),
      docsOnNewLine,
      Option(customName),
      Option(customDoc),
      sorted
    )
  }

  /** binary compatibility shim. */
  private[mainargs] def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customName: String,
      customDoc: String
  ): String = helpText(totalWidth, docsOnNewLine, customName, customDoc, sorted = false)

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
      customDoc: String = null
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
      customDoc
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
      customDoc: String = null
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
      customDoc
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
      sorted: Boolean = false
  ): Either[String, T] = {
    if (autoPrintHelpAndExit.nonEmpty && args.take(1) == Seq("--help")) {
      val (exitCode, outputStream) = autoPrintHelpAndExit.get
      outputStream.println(helpText(totalWidth, docsOnNewLine, customName, customDoc, sorted))
      Compat.exit(exitCode)
    } else constructRaw(args, allowPositional, allowRepeats) match {
      case Result.Success(v) => Right(v)
      case f: Result.Failure =>
        Left(
          Renderer.renderResult(
            mains.main,
            f,
            totalWidth,
            printHelpOnExit,
            docsOnNewLine,
            Option(customName),
            Option(customDoc),
            sorted
          )
        )
    }
  }

  /** binary compatibility shim. */
  private[mainargs] def constructEither(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customName: String,
      customDoc: String
  ): Either[String, T] = constructEither(
    args,
    allowPositional,
    allowRepeats,
    totalWidth,
    printHelpOnExit,
    docsOnNewLine,
    autoPrintHelpAndExit,
    customName,
    customDoc,
    sorted = false
  )

  def constructRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false
  ): Result[T] = {
    Invoker.construct[T](mains, args, allowPositional, allowRepeats)
  }
}
