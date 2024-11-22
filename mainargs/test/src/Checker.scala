package mainargs

class Checker[B](
    val parser: ParserForMethods[B],
    allowPositional: Boolean,
    nameMapper: String => Option[String] = Util.kebabCaseNameMapper
) {
  val mains = parser.mains
  def parseInvoke(input: List[String]) = {
    parser.runRaw(input, allowPositional = allowPositional, nameMapper = nameMapper)
  }
  def apply[T](input: List[String], expected: Result[T]) = {
    val result = parseInvoke(input)
    utest.assert(result == expected)
  }
}
