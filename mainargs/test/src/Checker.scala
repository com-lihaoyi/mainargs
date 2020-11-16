package mainargs

class Checker[B](val parser: ParserForMethods[B], allowPositional: Boolean){
  val mains = parser.mains
  def parseInvoke(input: List[String]) = {
    parser.runRaw(input, allowPositional = allowPositional)
  }
  def apply[T](input: List[String],
               expected: Result[T]) = {
    val result = parseInvoke(input)
    utest.assert(result == expected)
  }
}