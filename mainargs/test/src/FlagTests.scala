package mainargs
import utest._


object FlagTests extends TestSuite{

  object Base{
    @main
    def flaggy(@arg(flag = true) a: Boolean = false,
               b: Boolean,
               @arg(flag = true) c: Boolean = false) = a || b || c
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {
    test - check(
      List("--b", "true"), Result.Success(true)
    )
    test - check(
      List("--b", "false"), Result.Success(false)
    )

    test - check(
      List("--a", "--b", "false"), Result.Success(true)
    )

    test - check(
      List("--c", "--b", "false"), Result.Success(true)
    )

    test - check(
      List("--a", "--c", "--b", "false"), Result.Success(true)
    )

  }
}
