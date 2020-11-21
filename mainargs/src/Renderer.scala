package mainargs

import java.io.{PrintWriter, StringWriter}

object Renderer {

  def getLeftColWidth[T, B](items: Seq[ArgSig[T, B]]) = {
    if (items.isEmpty) 0
    else items
      .map(x =>
        x.name.length + 2 + // name and --
        x.shortName.fold (0) (_ => 3) + // -c and the separating whitespace
        (if (x.flag || x.typeString == "") 0 else x.typeString.size + 3) // "" or " <str>"
      )
      .max
  }

  val newLine = System.lineSeparator()
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)
  def renderArgShort[B](arg: ArgSig[_, B]) = {
    val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
    val typeSuffix = if (arg.flag) "" else s" <${arg.typeString}>"
    s"$shortPrefix--${arg.name}$typeSuffix"
  }

  def renderLeftoverArgShort[B](arg: LeftoverArgSig[_, B]) = {
    s"${arg.name} <${arg.reader.shortName}>..."
  }

  def renderArg[B](arg: AnyArgSig.Terminal[_, B],
                   leftOffset: Int,
                   wrappedWidth: Int): (String, String) = {
    val wrapped = softWrap(arg.doc.getOrElse(""), leftOffset, wrappedWidth - leftOffset)
    val renderedArg = arg match{
      case a: LeftoverArgSig[_, B] => renderLeftoverArgShort(a)
      case a: ArgSig[_, B] => renderArgShort(a)
    }
    (renderedArg, wrapped)
  }

  def formatMainMethods[B](mainMethods: Seq[MainData[_, B]], totalWidth: Int, docsOnNewLine: Boolean) = {
    val leftColWidth = getLeftColWidth(mainMethods.flatMap(_.argSigs))
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

  def formatMainMethodSignature[B](main: MainData[_, B],
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
  def renderEarlyError(result: Result.Error.Early) = result match {
    case Result.Error.Early.NoMainMethodsDetected() =>
      "No @main methods declared"
    case Result.Error.Early.SubcommandNotSpecified(options) =>
      "Need to specify a sub command: " + options.mkString(", ")
    case Result.Error.Early.UnableToFindSubcommand(token) =>
      s"Unable to find subcommand: " + token
    case Result.Error.Early.SubcommandSelectionDashes(token) =>
        "To select a subcommand to run, you don't need --s." + Renderer.newLine +
        s"Did you mean `${token.drop(2)}` instead of `$token`?"
  }
  def renderResult[B, T](main: MainData[_, B],
                         result: Result[T],
                         totalWidth: Int,
                         printHelpOnError: Boolean,
                         docsOnNewLine: Boolean): Either[String, T] = {

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
      case Result.Success(x) => Right(x)
      case err: Result.Error.Early => Left(renderEarlyError(err))
      case Result.Error.Exception(t) =>
        val s = new StringWriter()
        val ps = new PrintWriter(s)
        t.printStackTrace(ps)
        ps.close()
        Left(s.toString)
      case Result.Error.MismatchedArguments(missing, unknown, duplicate, incomplete) =>
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
                  s"Duplicate arguments for (--${sig.name}: ${sig.typeString}): " +
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

        Left(
          Renderer.normalizeNewlines(
            s"""$missingStr$unknownStr$duplicateStr$incompleteStr${expectedMsg()}
               |""".stripMargin
          )
        )

      case Result.Error.InvalidArguments(x) =>
        val argumentsStr = pluralize("argument", x.length)
        val thingies = x.map{
          case Result.ParamError.Exception(p, vs, ex) =>
            val literalV = vs.map(Util.literalize(_)).mkString(" ")
            val rendered = p match{
            case a: LeftoverArgSig[_, B] => renderLeftoverArgShort(a)
            case a: ArgSig[_, B] => renderArgShort(a)
          }
            s"$rendered = $literalV failed to parse with $ex"
          case Result.ParamError.DefaultFailed(p, ex) =>
            s"${Renderer.renderArgShort(p)}'s default value failed to evaluate with $ex"
        }

        Left(
          Renderer.normalizeNewlines(
            s"""The following $argumentsStr failed to parse:
               |${thingies.mkString(Renderer.newLine)}
               |${expectedMsg()}
            """.stripMargin
          )
        )
    }
  }
}
