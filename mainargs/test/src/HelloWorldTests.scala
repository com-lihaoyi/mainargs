package mainargs
import utest._

object HelloWorldTests extends TestSuite {

  object Main {
    @main
    def run(
        @arg(short = 'f', doc = "String to print repeatedly")
        foo: String,
        @arg(name = "my-num", doc = "How many times to print string")
        myNum: Int = 2,
        @arg(doc = "Example flag")
        bool: Flag
    ) = {
      foo * myNum + " " + bool.value
    }
  }

  val tests = Tests {
    test("explicit") {
      ParserForMethods(Main).runOrThrow(Array("--foo", "bar", "--my-num", "3")) ==>
        "barbarbar false"
    }
    test("short") {
      ParserForMethods(Main).runOrThrow(Array("-f", "bar")) ==>
        "barbar false"
    }
    test("default") {
      ParserForMethods(Main).runOrThrow(Array("--foo", "bar")) ==>
        "barbar false"
    }
    test("positional") {
      ParserForMethods(Main).runOrThrow(Array("qux", "4"), allowPositional = true) ==>
        "quxquxquxqux false"
    }
    test("flags") {
      ParserForMethods(Main).runOrThrow(Array("qux", "4", "--bool"), allowPositional = true) ==>
        "quxquxquxqux true"
    }
    test("either") {
      ParserForMethods(Main).runEither(Array("qux", "4", "--bool"), allowPositional = true) ==>
        Right("quxquxquxqux true")
    }

    test("equalsSyntax") {
      ParserForMethods(Main).runOrThrow(Array("--foo=bar", "--my-num=3")) ==>
        "barbarbar false"

      // --foo=bar syntax still works when there's an `=` on the right
      ParserForMethods(Main).runOrThrow(Array("--foo=bar=qux")) ==>
        "bar=quxbar=qux false"

      // -f=bar syntax for short names
      ParserForMethods(Main).runOrThrow(Array("-f=bar")) ==>
        "barbar false"

      // -f=bar syntax doesn't work for flags
      ParserForMethods(Main).runEither(Array("-f=bar", "--bool=true")) ==>
         Left("""Unknown argument: "--bool=true"
                |Expected Signature: run
                |  -f --foo <str>  String to print repeatedly
                |  --my-num <int>  How many times to print string
                |  --bool          Example flag
                |
                |""".stripMargin)
    }
  }
}
