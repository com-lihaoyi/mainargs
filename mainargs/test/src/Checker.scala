package mainargs

class Checker(allowPositional: Boolean){
  def parseInvoke[T](base: T, main: Mains[T], input: List[String]) = {
    Parser(input, allowPositional = allowPositional).runRaw[T](main)
  }
  def apply[B, T](base: B,
                  main: Mains[B],
                  input: List[String],
                  expected: Result[T]) = {
    val result = parseInvoke(base, main, input)
    utest.assert(result == expected)
  }
}