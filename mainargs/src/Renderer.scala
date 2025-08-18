package mainargs

import java.io.{PrintWriter, StringWriter}
import scala.math

object Renderer {

  def getLeftColWidth(items: Seq[ArgSig]): Int = getLeftColWidth(items, Util.kebabCaseNameMapper)
  def getLeftColWidth(items: Seq[ArgSig], nameMapper: String => Option[String]): Int = {
    if (items.isEmpty) 0
    else items.map(renderArgShort(_, nameMapper).length).max
  }

  val newLine = System.lineSeparator()

  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)

  def renderArgShort(arg: ArgSig): String = renderArgShort(arg, Util.nullNameMapper)

  def renderArgShort(arg: ArgSig, nameMapper: String => Option[String]): String = arg.reader match {
    case r: TokensReader.Flag =>
      val shortPrefix = arg.shortName.map(c => s"-$c")
      val nameSuffix = arg.longName(nameMapper).map(s => s"--$s")
      (shortPrefix ++ nameSuffix).mkString(" ")

    case r: TokensReader.Simple[_] =>
      val shortPrefix = arg.shortName.map(c => s"-$c")
      val typeSuffix = s"<${r.shortName}>"
      val nameSuffix =
        if (arg.positional) arg.longName(nameMapper) else arg.longName(nameMapper).map(s => s"--$s")
      (shortPrefix ++ nameSuffix ++ Seq(typeSuffix)).mkString(" ")

    case r: TokensReader.Leftover[_, _] =>
      s"${arg.longName(nameMapper).get} <${r.shortName}>..."
  }

  /**
   * Returns a `Some[string]` with the sortable string or a `None` if it is an leftover.
   */
  private def sortableName(arg: ArgSig, nameMapper: String => Option[String]): Option[String] =
    arg match {
      case arg: ArgSig if arg.reader.isLeftover => None

      case a: ArgSig =>
        a.shortName.map(_.toString).orElse(a.longName(nameMapper)).orElse(Some(""))
      case a: ArgSig =>
        a.longName(nameMapper)
    }

  object ArgOrd extends math.Ordering[ArgSig] {
    override def compare(x: ArgSig, y: ArgSig): Int =
      (sortableName(x, Util.nullNameMapper), sortableName(y, Util.nullNameMapper)) match {
        case (None, None) => 0 // don't sort leftovers
        case (None, Some(_)) => 1 // keep left overs at the end
        case (Some(_), None) => -1 // keep left overs at the end
        case (Some(l), Some(r)) => l.compare(r)
      }
  }
  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def renderArg(
      arg: ArgSig,
      leftOffset: Int,
      wrappedWidth: Int
  ): (String, String) = renderArg(arg, leftOffset, wrappedWidth, Util.kebabCaseNameMapper)

  def renderArg(
      arg: ArgSig,
      leftOffset: Int,
      wrappedWidth: Int,
      nameMapper: String => Option[String]
  ): (String, String) = {
    val wrapped = softWrap(arg.doc.getOrElse(""), leftOffset, wrappedWidth - leftOffset)
    (renderArgShort(arg, nameMapper), wrapped)
  }

  def formatMainMethods(
      mainMethods: Seq[MainData[_, _]],
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String],
      sorted: Boolean
  ): String = formatMainMethods(
    mainMethods,
    totalWidth,
    docsOnNewLine,
    customNames,
    customDocs,
    sorted,
    Util.kebabCaseNameMapper
  )

  def formatMainMethods(
      mainMethods: Seq[MainData[_, _]],
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String],
      sorted: Boolean,
      nameMapper: String => Option[String]
  ): String = {
    val flattenedAll: Seq[ArgSig] =
      mainMethods.map(_.flattenedArgSigs)
        .flatten
        .map(_._1)

    val leftColWidth = getLeftColWidth(flattenedAll, nameMapper)
    mainMethods match {
      case Seq() => ""
      case Seq(main) =>
        Renderer.formatMainMethodSignature(
          main,
          0,
          totalWidth,
          leftColWidth,
          docsOnNewLine,
          customNames.get(main.name(nameMapper)),
          customDocs.get(main.name(nameMapper)),
          sorted,
          nameMapper
        )
      case _ =>
        val methods =
          for (main <- mainMethods)
            yield formatMainMethodSignature(
              main,
              2,
              totalWidth,
              leftColWidth,
              docsOnNewLine,
              customNames.get(main.name(nameMapper)),
              customDocs.get(main.name(nameMapper)),
              sorted,
              nameMapper
            )

        normalizeNewlines(
          s"""Available subcommands:
             |
             |${methods.mkString(newLine)}""".stripMargin
        )
    }
  }

  @deprecated("Use other overload instead", "mainargs after 0.3.0")
  def formatMainMethods(
      mainMethods: Seq[MainData[_, _]],
      totalWidth: Int,
      docsOnNewLine: Boolean,
      customNames: Map[String, String],
      customDocs: Map[String, String]
  ): String = formatMainMethods(
    mainMethods,
    totalWidth,
    docsOnNewLine,
    customNames,
    customDocs,
    sorted = true,
    Util.kebabCaseNameMapper
  )

  def formatMainMethodSignature(
      main: MainData[_, _],
      leftIndent: Int,
      totalWidth: Int,
      leftColWidth: Int,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String],
      sorted: Boolean,
      nameMapper: String => Option[String]
  ): String = {

    val argLeftCol = if (docsOnNewLine) leftIndent + 8 else leftColWidth + leftIndent + 2 + 2

    val sortedArgs =
      if (sorted) main.renderedArgSigs.sorted(ArgOrd)
      else main.renderedArgSigs

    val args = sortedArgs.map(renderArg(_, argLeftCol, totalWidth, nameMapper))

    val leftIndentStr = " " * leftIndent

    def formatArg(lhs: String, rhs: String) = {
      val lhsPadded = lhs.padTo(leftColWidth, ' ')
      val rhsPadded = rhs.linesIterator.mkString(newLine)
      if (rhs.isEmpty) s"$leftIndentStr  $lhs"
      else if (docsOnNewLine) {
        s"$leftIndentStr  $lhs\n$leftIndentStr        $rhsPadded"
      } else {
        s"$leftIndentStr  $lhsPadded  $rhsPadded"
      }
    }
    val argStrings = for ((lhs, rhs) <- args) yield formatArg(lhs, rhs)

    val mainDocSuffix = customDoc.orElse(main.doc) match {
      case Some(d) => newLine + leftIndentStr + softWrap(d, leftIndent, totalWidth)
      case None => ""
    }
    s"""$leftIndentStr${customName.getOrElse(main.name(nameMapper))}$mainDocSuffix
       |${argStrings.map(_ + newLine).mkString}""".stripMargin
  }

  @deprecated("Use other overload instead", "mainargs after 0.3.0")
  def formatMainMethodSignature(
      main: MainData[_, _],
      leftIndent: Int,
      totalWidth: Int,
      leftColWidth: Int,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String]
  ): String = formatMainMethodSignature(
    main,
    leftIndent,
    totalWidth,
    leftColWidth,
    docsOnNewLine,
    customName,
    customDoc,
    sorted = true
  )

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def formatMainMethodSignature(
      main: MainData[_, _],
      leftIndent: Int,
      totalWidth: Int,
      leftColWidth: Int,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String],
      sorted: Boolean
  ): String = formatMainMethodSignature(
    main,
    leftIndent,
    totalWidth,
    leftColWidth,
    docsOnNewLine,
    customName,
    customDoc,
    sorted,
    Util.kebabCaseNameMapper
  )

  def softWrap(s: String, leftOffset: Int, maxWidth: Int) = {
    if (s.isEmpty) s
    else {
      val oneLine = s.linesIterator.mkString(" ").split(' ').filter(_.nonEmpty)

      lazy val indent = " " * leftOffset

      val output = new StringBuilder(oneLine.head)
      var currentLineWidth = oneLine.head.length
      for (chunk <- oneLine.tail) {
        val addedWidth = currentLineWidth + chunk.length + 1
        if (addedWidth > maxWidth) {
          output.append(newLine + indent)
          output.append(chunk)
          currentLineWidth = chunk.length
        } else {
          currentLineWidth = addedWidth
          output.append(' ')
          output.append(chunk)
        }
      }
      output.mkString
    }
  }

  def pluralize(s: String, n: Int) = if (n == 1) s else s + "s"
  def renderEarlyError(result: Result.Failure.Early) = result match {
    case Result.Failure.Early.NoMainMethodsDetected() =>
      "No @main methods declared"
    case Result.Failure.Early.SubcommandNotSpecified(options) =>
      "Need to specify a sub command: " + options.mkString(", ")
    case Result.Failure.Early.UnableToFindSubcommand(options, token) =>
      s"Unable to find subcommand: $token, available subcommands: ${options.mkString(", ")}"
    case Result.Failure.Early.SubcommandSelectionDashes(token) =>
      "To select a subcommand to run, you don't need --s." + Renderer.newLine +
        s"Did you mean `${token.drop(2)}` instead of `$token`?"
  }

  @deprecated("Binary Compatibility Shim", "mainargs 0.6.0")
  def renderResult(
      main: MainData[_, _],
      result: Result.Failure,
      totalWidth: Int,
      printHelpOnError: Boolean,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String],
      sorted: Boolean
  ): String = renderResult(
    main,
    result,
    totalWidth,
    printHelpOnError,
    docsOnNewLine,
    customName,
    customDoc,
    sorted,
    Util.kebabCaseNameMapper
  )
  def renderResult(
      main: MainData[_, _],
      result: Result.Failure,
      totalWidth: Int,
      printHelpOnError: Boolean,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String],
      sorted: Boolean,
      nameMapper: String => Option[String]
  ): String = {

    def expectedMsg() = {
      if (printHelpOnError) {
        val leftColWidth = getLeftColWidth(main.renderedArgSigs, nameMapper)
        "Expected Signature: " +
          Renderer.formatMainMethodSignature(
            main,
            0,
            totalWidth,
            leftColWidth,
            docsOnNewLine,
            customName,
            customDoc,
            sorted,
            nameMapper
          )
      } else ""
    }
    result match {
      case err: Result.Failure.Early => renderEarlyError(err)
      case Result.Failure.Exception(t) =>
        val s = new StringWriter()
        val ps = new PrintWriter(s)
        t.printStackTrace(ps)
        ps.close()
        s.toString
      case Result.Failure.MismatchedArguments(missing, unknown, duplicate, incomplete) =>
        val missingStr =
          if (missing.isEmpty) ""
          else {
            val chunks = missing.map(renderArgShort(_, nameMapper))

            val argumentsStr = pluralize("argument", chunks.length)
            s"Missing $argumentsStr: ${chunks.mkString(" ")}" + Renderer.newLine
          }

        val unknownStr =
          if (unknown.isEmpty) ""
          else {
            val argumentsStr = pluralize("argument", unknown.length)
            s"Unknown $argumentsStr: " + unknown.map(Util.literalize(_)).mkString(
              " "
            ) + Renderer.newLine
          }

        val duplicateStr =
          if (duplicate.isEmpty) ""
          else {
            val lines =
              for ((sig, options) <- duplicate)
                yield {
                  s"Duplicate arguments for ${renderArgShort(sig, nameMapper)}: " +
                    options.map(Util.literalize(_)).mkString(" ") + Renderer.newLine
                }

            lines.mkString

          }
        val incompleteStr = incomplete match {
          case None => ""
          case Some(sig) =>
            s"Incomplete argument ${renderArgShort(sig, nameMapper)} is missing a corresponding value" +
              Renderer.newLine

        }

        Renderer.normalizeNewlines(
          s"""$missingStr$unknownStr$duplicateStr$incompleteStr${expectedMsg()}
             |""".stripMargin
        )

      case Result.Failure.InvalidArguments(x) =>
        val thingies = x.map {
          case Result.ParamError.Failed(p, vs, errMsg) =>
            val literalV = vs.map(Util.literalize(_)).mkString(" ")
            s"Invalid argument ${renderArgShort(p, nameMapper)} failed to parse $literalV due to $errMsg"
          case Result.ParamError.Exception(p, vs, ex) =>
            val literalV = vs.map(Util.literalize(_)).mkString(" ")
            s"Invalid argument ${renderArgShort(p, nameMapper)} failed to parse $literalV due to $ex"
          case Result.ParamError.DefaultFailed(p, ex) =>
            s"Invalid argument ${renderArgShort(p, nameMapper)}'s default value failed to evaluate with $ex"
        }

        Renderer.normalizeNewlines(
          s"""${thingies.mkString(Renderer.newLine)}
             |${expectedMsg()}
          """.stripMargin
        )
    }
  }

  @deprecated("Use other overload instead", "mainargs after 0.3.0")
  def renderResult(
      main: MainData[_, _],
      result: Result.Failure,
      totalWidth: Int,
      printHelpOnError: Boolean,
      docsOnNewLine: Boolean,
      customName: Option[String],
      customDoc: Option[String]
  ): String = renderResult(
    main,
    result,
    totalWidth,
    printHelpOnError,
    docsOnNewLine,
    customName,
    customDoc,
    sorted = true
  )
}
