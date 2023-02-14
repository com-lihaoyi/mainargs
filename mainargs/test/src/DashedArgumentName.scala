package mainargs
import utest._

object DashedArgumentName extends TestSuite {

  object Base {
    @main
    def `opt-for-18+name`(`opt-for-18+arg`: Boolean) = `opt-for-18+arg`
    @main
    def `opt-for-29+name`(`opt-for-29+arg`: Boolean) = `opt-for-29+arg`
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {
    test - check(
      List("opt-for-18+name", "--opt-for-18+arg", "true"),
      Result.Success(true)
    )
    test - check(
      List("opt-for-18+name", "--opt-for-18+arg", "false"),
      Result.Success(false)
    )
    test - check(
      List("opt-for-29+name", "--opt-for-29+arg", "true"),
      Result.Success(true)
    )
    test - check(
      List("opt-for-29+name", "--opt-for-29+arg", "false"),
      Result.Success(false)
    )
  }
}
