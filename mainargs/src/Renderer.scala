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
      case Some(f) => " (default " + Util.literalize(f(base).toString) + ")"
      case None => ""
    }
    val docSuffix = arg.doc match{
      case Some(d) => ": " + d
      case None => ""
    }
    val wrapped = softWrap(
      arg.typeString + suffix + docSuffix,
      leftOffset,
      wrappedWidth - leftOffset
    )
    (renderArgShort(arg), wrapped)
  }
  def formatMainMethods[T](base: T, mainMethods: Seq[EntryPoint[T]]) = {
    if (mainMethods.isEmpty) ""
    else{
      val leftColWidth = getLeftColWidth(mainMethods.flatMap(_.argSignatures))

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
    val args = main.argSignatures.map(renderArg(base, _, leftColWidth + leftIndent + 2 + 2, 80))

    val leftIndentStr = " " * leftIndent
    val argStrings =
      for((lhs, rhs) <- args)
        yield {
          val lhsPadded = lhs.padTo(leftColWidth, ' ')
          val rhsPadded = Predef.augmentString(rhs).lines.mkString(newLine)
          s"$leftIndentStr  $lhsPadded  $rhsPadded"
        }
    val mainDocSuffix = main.doc match{
      case Some(d) => newLine + leftIndentStr + softWrap(d, leftIndent, 80)
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

}
