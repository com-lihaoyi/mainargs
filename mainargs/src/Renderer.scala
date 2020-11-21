package mainargs

import java.io.{PrintStream, PrintWriter, StringWriter}

object Renderer {
  val newLine = System.lineSeparator()
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)
  def renderArgShort[B](arg: ArgSig[_, B]) = {
    val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
    val typeSuffix = if (arg.flag) "" else s" <${arg.typeString}>"
    if (arg.varargs) s"${arg.name} ..."
    else s"$shortPrefix--${arg.name}$typeSuffix"
  }

  def renderArg[B](arg: ArgSig[_, B],
                   leftOffset: Int,
                   wrappedWidth: Int): (String, String) = {
    val docSuffix = arg.doc.getOrElse("")
    val wrapped = softWrap(
      docSuffix,
      leftOffset,
      wrappedWidth - leftOffset
    )
    (renderArgShort(arg), wrapped)
  }

  def formatMainMethods[B](mainMethods: Seq[MainData[_, B]], totalWidth: Int) = {
    if (mainMethods.isEmpty) ""
    else{
      val methods =
        for(main <- mainMethods) yield formatMainMethodSignature(main, 2, totalWidth)

      normalizeNewlines(
        s"""
           |
           |Available subcommands:
           |
           |${methods.mkString(newLine)}""".stripMargin
      )
    }
  }

  def formatMainMethodSignature[B](main: MainData[_, B],
                                   leftIndent: Int,
                                   totalWidth: Int) = {
    // +2 for space on right of left col
    val args = main.argSigs.map(renderArg(_, leftIndent + 8, totalWidth))

    val leftIndentStr = " " * leftIndent
    val argStrings = for((lhs, rhs) <- args) yield {
      val rhsPadded = Predef.augmentString(rhs).lines.mkString(newLine)
      s"$leftIndentStr  $lhs\n$leftIndentStr        $rhsPadded"
    }
    val mainDocSuffix = main.doc match{
      case Some(d) => newLine + softWrap(d, leftIndent, totalWidth)
      case None => ""
    }

    s"""$leftIndentStr${main.name}$mainDocSuffix
       |${argStrings.map(_ + newLine).mkString("\n")}""".stripMargin
  }

  def softWrap(s: String, leftOffset: Int, maxWidth: Int) = {
    val oneLine = Predef.augmentString(s).lines.mkString(" ").split(' ')

    lazy val indent = " " * leftOffset

    val output = new StringBuilder(oneLine.head)
    var currentLineWidth = oneLine.head.length
    for(chunk <- oneLine.tail){
      val addedWidth = currentLineWidth + chunk.length + 1
      if (addedWidth > maxWidth){
        output.append(newLine + indent)
        output.append(chunk)
        currentLineWidth = chunk.length
      } else{
        currentLineWidth = addedWidth
        output.append(' ')
        output.append(chunk)
      }
    }
    output.mkString
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
                         totalWidth: Int): Either[String, T] = {

    def expectedMsg() = {
      Renderer.formatMainMethodSignature(main, 0, totalWidth)
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
            val chunks =
              for (x <- missing)
                yield "--" + x.name + ": " + x.typeString

            val argumentsStr = pluralize("argument", chunks.length)
            s"Missing $argumentsStr: (${chunks.mkString(", ")})" + Renderer.newLine
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
            s"""$missingStr$unknownStr$duplicateStr$incompleteStr
               |Arguments provided did not match expected signature:
               |
               |${expectedMsg()}
               |""".stripMargin
          )
        )

      case Result.Error.InvalidArguments(x) =>
        val argumentsStr = pluralize("argument", x.length)
        val thingies = x.map{
          case Result.ParamError.Exception(p, vs, ex) =>
            val literalV = vs.map(Util.literalize(_)).mkString(" ")
            val rendered = {Renderer.renderArgShort(p)}
            s"$rendered: ${p.typeString} = $literalV failed to parse with $ex"
          case Result.ParamError.DefaultFailed(p, ex) =>
            s"${Renderer.renderArgShort(p)}'s default value failed to evaluate with $ex"
        }

        Left(
          Renderer.normalizeNewlines(
            s"""The following $argumentsStr failed to parse:
               |
               |${thingies.mkString(Renderer.newLine)}
               |
               |expected signature:
               |
               |${expectedMsg()}
            """.stripMargin
          )
        )
    }
  }
}
