package mainargs

import java.io.{PrintWriter, StringWriter}

object Renderer {

  def getLeftColWidth(items: Seq[ArgSig.Terminal[_, _]]) = {
    if (items.isEmpty) 0
    else items
      .map {
        case x: ArgSig.Flag[_] =>
          x.name.length + 2 + // name and --
          x.shortName.fold (0) (_ => 3) // -c and the separating whitespace
        case x: ArgSig.Simple[_, _] =>
          x.name.length + 2 + // name and --
          x.shortName.fold (0) (_ => 3) + // -c and the separating whitespace
          x.typeString.size + 3 // "" or " <str>"
        case x: ArgSig.Leftover[_, _] =>
          x.name.size + 1 + x.reader.shortName.size + 2 + 3
      }
      .max
  }

  val newLine = System.lineSeparator()
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)
  def renderArgShort(arg: ArgSig.Terminal[_, _]) = arg match{
    case arg: ArgSig.Flag[_] =>
      val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
      s"$shortPrefix--${arg.name}"
    case arg: ArgSig.Simple[_, _] =>
      val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
      val typeSuffix = s" <${arg.typeString}>"
      s"$shortPrefix--${arg.name}$typeSuffix"
    case arg: ArgSig.Leftover[_, _] =>
      s"${arg.name} <${arg.reader.shortName}>..."
  }


  def renderArg(arg: ArgSig.Terminal[_, _],
                leftOffset: Int,
                wrappedWidth: Int): (String, String) = {
    val wrapped = softWrap(arg.doc.getOrElse(""), leftOffset, wrappedWidth - leftOffset)
    (renderArgShort(arg), wrapped)
  }

  def formatMainMethods(mainMethods: Seq[MainData[_, _]], totalWidth: Int, docsOnNewLine: Boolean) = {
    val flattenedAll: Seq[ArgSig.Terminal[_, _]] =
      mainMethods.map(_.argSigs)
        .flatten
    val leftColWidth = getLeftColWidth(flattenedAll)
    if (mainMethods.isEmpty) ""
    else{
      val methods =
        for(main <- mainMethods)
        yield formatMainMethodSignature(main, 2, totalWidth, leftColWidth, docsOnNewLine)

      normalizeNewlines(
        s"""Available subcommands:
           |
           |${methods.mkString(newLine)}""".stripMargin
      )
    }
  }

  def formatMainMethodSignature(main: MainData[_, _],
                                leftIndent: Int,
                                totalWidth: Int,
                                leftColWidth: Int,
                                docsOnNewLine: Boolean) = {

    val argLeftCol = if (docsOnNewLine) leftIndent + 8 else leftColWidth + leftIndent + 2 + 2
    val args =
      main.argSigs.map(renderArg(_, argLeftCol, totalWidth)) ++
      main.leftoverArgSig.map(renderArg(_, argLeftCol, totalWidth))

    val leftIndentStr = " " * leftIndent

    def formatArg(lhs: String, rhs: String) = {
      val lhsPadded = lhs.padTo(leftColWidth, ' ')
      val rhsPadded = rhs.linesIterator.mkString(newLine)
      if (rhs.isEmpty) s"$leftIndentStr  $lhs"
      else if (docsOnNewLine){
        s"$leftIndentStr  $lhs\n$leftIndentStr        $rhsPadded"
      }else {
        s"$leftIndentStr  $lhsPadded  $rhsPadded"
      }
    }
    val argStrings = for ((lhs, rhs) <- args) yield formatArg(lhs, rhs)

    val mainDocSuffix = main.doc match{
      case Some(d) => newLine + leftIndentStr + softWrap(d, leftIndent, totalWidth)
      case None => ""
    }
    s"""$leftIndentStr${main.name}$mainDocSuffix
       |${argStrings.map(_ + newLine).mkString}""".stripMargin
  }

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
    case Result.Failure.Early.UnableToFindSubcommand(token) =>
      s"Unable to find subcommand: " + token
    case Result.Failure.Early.SubcommandSelectionDashes(token) =>
        "To select a subcommand to run, you don't need --s." + Renderer.newLine +
        s"Did you mean `${token.drop(2)}` instead of `$token`?"
  }
  def renderResult(main: MainData[_, _],
                   result: Result.Failure,
                   totalWidth: Int,
                   printHelpOnError: Boolean,
                   docsOnNewLine: Boolean): String = {

    def expectedMsg() = {
      if (printHelpOnError) {
        val leftColWidth = getLeftColWidth(main.argSigs)
        "Expected Signature: " +
        Renderer.formatMainMethodSignature(
          main,
          0,
          totalWidth,
          leftColWidth,
          docsOnNewLine
        )
      }
      else ""
    }
    result match{
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
            val chunks = missing.map(renderArgShort(_))

            val argumentsStr = pluralize("argument", chunks.length)
            s"Missing $argumentsStr: ${chunks.mkString(" ")}" + Renderer.newLine
          }


        val unknownStr =
          if (unknown.isEmpty) ""
          else {
            val argumentsStr = pluralize("argument", unknown.length)
            s"Unknown $argumentsStr: " + unknown.map(Util.literalize(_)).mkString(" ") + Renderer.newLine
          }

        val duplicateStr =
          if (duplicate.isEmpty) ""
          else {
            val lines =
              for ((sig, options) <- duplicate)
                yield {
                  s"Duplicate arguments for (${renderArgShort(sig)}): " +
                  options.map(Util.literalize(_)).mkString(" ") + Renderer.newLine
                }

            lines.mkString

          }
        val incompleteStr = incomplete match{
          case None => ""
          case Some(sig) =>
            s"Option (--${sig.name}: ${sig.typeString}) is missing a corresponding value" +
              Renderer.newLine

        }

        Renderer.normalizeNewlines(
          s"""$missingStr$unknownStr$duplicateStr$incompleteStr${expectedMsg()}
             |""".stripMargin
        )

      case Result.Failure.InvalidArguments(x) =>
        val argumentsStr = pluralize("argument", x.length)
        val thingies = x.map{
          case Result.ParamError.Exception(p, vs, ex) =>
            val literalV = vs.map(Util.literalize(_)).mkString(" ")

            s"${renderArgShort(p)} = $literalV failed to parse with $ex"
          case Result.ParamError.DefaultFailed(p, ex) =>
            s"${renderArgShort(p)}'s default value failed to evaluate with $ex"
        }

        Renderer.normalizeNewlines(
          s"""The following $argumentsStr failed to parse:
             |${thingies.mkString(Renderer.newLine)}
             |${expectedMsg()}
          """.stripMargin
        )
    }
  }
}
