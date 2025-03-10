package mainargs

import acyclic.skipped

import scala.language.experimental.macros
import java.io.PrintStream

object ParserForMethods extends ParserForMethodsCompanionVersionSpecific
class ParserForMethods[B](val mains: MethodMains[B]) {
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String],
      sorted: Boolean): String = {
    helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted, Util.kebabCaseNameMapper)
  }

  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customNames: Map[String, String] = Map(),
      customDocs: Map[String, String] = Map(),
      sorted: Boolean = true,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper,
      mainMethods: Seq[MainData[Any, B]] = mains.value
  ): String = {
    Renderer.formatMainMethods(
      mainMethods,
      totalWidth,
      docsOnNewLine,
      customNames,
      customDocs,
      sorted,
      nameMapper
    )
  }

  def handlePrintAndExit(
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      args: Seq[String],
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String],
      sorted: Boolean,
      nameMapper: String => Option[String]
  ): Unit = {
    autoPrintHelpAndExit.foreach { case (exitCode, outputStream) =>
      val mainHelp = args.headOption.contains("--help")
      val subcommand = mains.value.find(main => args.headOption.contains(main.name(nameMapper)))
      val subcommandHelp = subcommand.isDefined && args.lift(1).contains("--help")

      if (mainHelp || subcommandHelp) {
        val text = if (mainHelp) {
          helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted, nameMapper)
        } else {
          helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted, nameMapper, subcommand.toSeq)
        }
        outputStream.println(text)
        Compat.exit(exitCode)
      }
    }
  }

  @deprecated("Binary compatibility shim, use other overload instead", "mainargs after 0.3.0")
  private[mainargs] def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String]
  ): String = helpText(totalWidth, docsOnNewLine, customNames, customDocs, sorted = true)

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
      customDocs: Map[String, String] = Map(),
      sorted: Boolean = true,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
      sorted,
      nameMapper
    ) match {
      case Left(msg) =>
        stderr.println(msg)
        Compat.exit(1)
      case Right(v) => v
    }
  }

  def runOrExit(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      stderr: PrintStream,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customNames: Map[String, String],
      customDocs: Map[String, String]
  ): Any = {
    runOrExit(
      args,
      allowPositional,
      allowRepeats,
      stderr,
      totalWidth,
      printHelpOnExit,
      docsOnNewLine,
      autoPrintHelpAndExit,
      customNames,
      customDocs,
      sorted = true,
      Util.kebabCaseNameMapper
    )
  }

  def runOrThrow(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customNames: Map[String, String],
      customDocs: Map[String, String],
  ): Any = runOrThrow(
    args,
    allowPositional,
    allowRepeats,
    totalWidth,
    printHelpOnExit,
    docsOnNewLine,
    autoPrintHelpAndExit,
    customNames,
    customDocs,
    Util.kebabCaseNameMapper
  )

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
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
      sorted: Boolean = true,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Either[String, Any] = {
    handlePrintAndExit(autoPrintHelpAndExit, args, totalWidth, docsOnNewLine, customNames, customDocs, sorted, nameMapper)
    runRaw0(args, allowPositional, allowRepeats, nameMapper) match {
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

  @deprecated("Binary compatibility shim, use other overload instead", "mainargs after 0.3.0")
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



  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def runEither(
                 args: Seq[String],
                 allowPositional: Boolean,
                 allowRepeats: Boolean,
                 totalWidth: Int,
                 printHelpOnExit: Boolean,
                 docsOnNewLine: Boolean,
                 autoPrintHelpAndExit: Option[(Int, PrintStream)],
                 customNames: Map[String, String],
                 customDocs: Map[String, String],
                 sorted: Boolean
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
    sorted,
    Util.kebabCaseNameMapper
  )
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def runRaw(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
  ): Result[Any] = runRaw(
    args, allowPositional, allowRepeats, Util.kebabCaseNameMapper
  )
  def runRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Result[Any] = {
    runRaw0(args, allowPositional, allowRepeats, nameMapper) match {
      case Left(err) => err
      case Right((main, res)) => res
    }
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def runRaw0(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
  ): Either[Result.Failure.Early, (MainData[_, B], Result[Any])] = runRaw0(
    args,
    allowPositional,
    allowRepeats,
    Util.kebabCaseNameMapper
  )

  def runRaw0(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customName: String,
      customDoc: String,
      sorted: Boolean): String = helpText(totalWidth, docsOnNewLine, customName, customDoc, sorted, Util.kebabCaseNameMapper)

  def helpText(
      totalWidth: Int = 100,
      docsOnNewLine: Boolean = false,
      customName: String = null,
      customDoc: String = null,
      sorted: Boolean = true,
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

  @deprecated("Binary compatibility shim, use other overload instead", "mainargs after 0.3.0")
  private[mainargs] def helpText(
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customName: String,
      customDoc: String
  ): String = helpText(totalWidth, docsOnNewLine, customName, customDoc, sorted = true)

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def constructOrExit(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      stderr: PrintStream,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customName: String,
      customDoc: String): T = constructOrExit(
    args,
    allowPositional,
    allowRepeats,
    stderr,
    totalWidth,
    printHelpOnExit,
    docsOnNewLine,
    autoPrintHelpAndExit,
    customName,
    customDoc,
    Util.kebabCaseNameMapper
  )

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
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
      allowPositional: Boolean,
      allowRepeats: Boolean,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customName: String,
      customDoc: String,
  ): T = constructOrThrow(
    args,
    allowPositional,
    allowRepeats,
    totalWidth,
    printHelpOnExit,
    docsOnNewLine,
    autoPrintHelpAndExit,
    customName,
    customDoc,
    Util.kebabCaseNameMapper
  )

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
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
      nameMapper
    ) match {
      case Left(msg) => throw new Exception(msg)
      case Right(v) => v
    }
  }

  def constructEither(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
      totalWidth: Int,
      printHelpOnExit: Boolean,
      docsOnNewLine: Boolean,
      autoPrintHelpAndExit: Option[(Int, PrintStream)],
      customName: String,
      customDoc: String,
      sorted: Boolean,
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
    sorted,
    Util.kebabCaseNameMapper
  )
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
      sorted: Boolean = true,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
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
      customDoc: String,
      nameMapper: String => Option[String]
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
    sorted = true,
    nameMapper = nameMapper
  )

  def constructRaw(
      args: Seq[String],
      allowPositional: Boolean,
      allowRepeats: Boolean,
  ): Result[T] = constructRaw(
    args,
    allowPositional,
    allowRepeats,
    nameMapper = Util.kebabCaseNameMapper
  )

  def constructRaw(
      args: Seq[String],
      allowPositional: Boolean = false,
      allowRepeats: Boolean = false,
      nameMapper: String => Option[String] = Util.kebabCaseNameMapper
  ): Result[T] = {
    Invoker.construct[T](this, args, allowPositional, allowRepeats, nameMapper)
  }
}
