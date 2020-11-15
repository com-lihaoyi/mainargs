package mainargs

class Checker(allowPositional: Boolean){
  def parseInvoke[T](base: T, entryPoint: EntryPoints[T], input: List[String]) = {
    Parser(input, allowPositional = allowPositional).runRaw[T](entryPoint)
  }
  def apply[B, T](base: B,
                  entryPoint: EntryPoints[B],
                  input: List[String],
                  expected: Result[T]) = {
    val result = parseInvoke(base, entryPoint, input)
    utest.assert(result == expected)
  }
}