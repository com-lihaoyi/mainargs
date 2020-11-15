package mainargs
object Renderer {
  val newLine = System.lineSeparator()
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)
  def renderArgShort[T](arg: ArgSig[T], base: Option[T]) = {
    val defaultSuffix = (base, arg.default) match{
      case (Some(b), Some(f)) => " (default " + Util.literalize(f(b).toString) + ") "
      case _ => ""
    }
    val shortPrefix = arg.shortName.fold("")(c => s"-$c ")
    val typeSuffix = if (arg.flag) "" else s" <${arg.typeString}>"
    if (arg.varargs) s"${arg.name} ..."
    else s"$shortPrefix--${arg.name}$typeSuffix$defaultSuffix"
  }

  def renderArg[T](base: T,
                   arg: ArgSig[T],
                   leftOffset: Int,
                   wrappedWidth: Int): (String, String) = {
    val docSuffix = arg.doc.getOrElse("")
    val wrapped = softWrap(
      docSuffix,
      leftOffset,
      wrappedWidth - leftOffset
    )
    (renderArgShort(arg, Some(base)), wrapped)
  }

  def formatMainMethods[T](base: T, mainMethods: Seq[EntryPoint[T]], totalWidth: Int) = {
    if (mainMethods.isEmpty) ""
    else{

      val methods =
        for(main <- mainMethods)
        yield formatMainMethodSignature(base, main, 2, totalWidth)

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
                                   totalWidth: Int) = {
    // +2 for space on right of left col
    val args = main.argSigs.map(renderArg(base, _, leftIndent + 8, totalWidth))

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

  def pluralize(s: String, n: Int) = if (n == 1) s else s + "s"
  def renderResult[T, V](base: () => T,
                         main: EntryPoint[T],
                         result: Result[V],
                         totalWidth: Int): Either[String, V] = {

    def expectedMsg() = {
      Renderer.formatMainMethodSignature(base(), main, 0, totalWidth)
    }
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
               |${expectedMsg()}
               |""".stripMargin
          )
        )

      case Result.Error.InvalidArguments(x) =>
        val argumentsStr = pluralize("argument", x.length)
        val thingies = x.map{
          case Result.ParamError.Exception(p, v, ex) =>
            val literalV = Util.literalize(v)
            val rendered = {Renderer.renderArgShort(p, None)}
            s"$rendered: ${p.typeString} = $literalV failed to parse with $ex"
          case Result.ParamError.DefaultFailed(p, ex) =>
            s"${Renderer.renderArgShort(p, None)}'s default value failed to evaluate with $ex"
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
