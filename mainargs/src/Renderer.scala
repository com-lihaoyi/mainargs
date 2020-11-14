package mainargs
object Renderer {
  val newLine = System.lineSeparator()
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)
  def getLeftColWidth[T](items: Seq[ArgSig[T]]) = {
    items.map(_.name.length + 2) match{
      case Nil => 0
      case x => x.max
    }
  }
  def renderArgShort[T](arg: ArgSig[T]) = {
    val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
    if (arg.varargs) s"[${arg.name} ...]"
    else if (arg.default.nonEmpty) s"[$shortPrefix--${arg.name}]"
    else s"$shortPrefix--${arg.name}"
  }
  def renderArg[T](base: T,
                   arg: ArgSig[T],
                   leftOffset: Int,
                   wrappedWidth: Int): (String, String) = {
    val suffix = arg.default match{
      case Some(f) => "(default " + Util.literalize(f(base).toString) + ") "
      case None => ""
    }
    val docSuffix = arg.doc.getOrElse("")
    val wrapped = softWrap(
      suffix + docSuffix,
      leftOffset,
      wrappedWidth - leftOffset
    )
    (renderArgShort(arg), wrapped)
  }
  def formatMainMethods[T](base: T, mainMethods: Seq[EntryPoint[T]]) = {
    if (mainMethods.isEmpty) ""
    else{
      val leftColWidth = getLeftColWidth(mainMethods.flatMap(_.argSigs))

      val methods =
        for(main <- mainMethods)
        yield formatMainMethodSignature(base, main, 2, leftColWidth)

      normalizeNewlines(
        s"""
           |
           |Available subcommands:
           |
           |${methods.mkString(newLine)}""".stripMargin
      )
    }
  }
  def formatMainMethodSignature[T](base: T,
                                   main: EntryPoint[T],
                                   leftIndent: Int,
                                   leftColWidth: Int) = {
    // +2 for space on right of left col
    val args = main.argSigs.map(renderArg(base, _, leftColWidth + leftIndent + 2 + 2, 80))

    val leftIndentStr = " " * leftIndent
    val argStrings =
      for((lhs, rhs) <- args)
        yield {
          val lhsPadded = lhs.padTo(leftColWidth, ' ')
          val rhsPadded = Predef.augmentString(rhs).lines.mkString(newLine)
          s"$leftIndentStr  $lhsPadded  $rhsPadded"
        }
    val mainDocSuffix = main.doc match{
      case Some(d) => newLine + softWrap(d, leftIndent, 80)
      case None => ""
    }

    s"""$leftIndentStr${main.name}$mainDocSuffix
       |${argStrings.map(_ + newLine).mkString}""".stripMargin
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

  def pluralize(s: String, n: Int) = {
    if (n == 1) s else s + "s"
  }
  def renderResult[T](base: T, main: EntryPoint[T], result: Result[_]) = {
    val leftColWidth = Renderer.getLeftColWidth(main.argSigs)

    val expectedMsg = Renderer.formatMainMethodSignature(base: T, main, 0, leftColWidth)
    result match{
      case Result.Success(x) => Right(x)
      case Result.Error.Exception(x) => Left(x.getStackTrace.mkString("\n"))
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
               |$expectedMsg
               |""".stripMargin
          )
        )

      case Result.Error.InvalidArguments(x) =>
        val argumentsStr = pluralize("argument", x.length)
        val thingies = x.map{
          case Result.ParamError.Invalid(p, v, ex) =>
            val literalV = Util.literalize(v)
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
               |$expectedMsg
            """.stripMargin
          )
        )
    }
  }

}
