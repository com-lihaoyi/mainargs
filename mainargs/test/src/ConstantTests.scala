package mainargs
import utest._

object ConstantTests extends TestSuite {

  case class Injected()
  implicit def InjectedTokensReader: TokensReader.Constant[Injected] =
    new TokensReader.Constant[Injected] {
      def read() = Right(new Injected())
    }
  object Base {
    @main
    def flaggy(a: Injected, b: Boolean) = a.toString + " " + b
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {
    test - check(
      List("-b", "true"),
      Result.Success("Injected() true")
    )
    test - check(
      List("-b", "false"),
      Result.Success("Injected() false")
    )
  }
}
