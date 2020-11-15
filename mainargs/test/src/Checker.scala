package mainargs

class Checker[B: Mains](allowPositional: Boolean){
  val mains = implicitly[Mains[B]]
  def parseInvoke(input: List[String]) = {
    Parser(input, allowPositional = allowPositional).runRaw[B]
  }
  def apply[B, T](input: List[String],
                  expected: Result[T]) = {
    val result = parseInvoke(input)
    utest.assert(result == expected)
  }
}