package mainargs
import utest._

object EqualsSyntaxTests extends TestSuite {

  object Main {
    @main
    def run(
        @arg(short = 'f', doc = "String to print repeatedly")
        foo: String,
        @arg(doc = "How many times to print string")
        myNum: Int = 2,
        @arg(doc = "Example flag")
        bool: Flag
    ) = {
      foo * myNum + " " + bool.value
    }
  }

  val tests = Tests {
    test("simple") {
      Parser(Main).runOrThrow(Array("--foo=bar", "--my-num=3")) ==>
        "barbarbar false"
    }
    test("multipleEquals") {
      // --foo=bar syntax still works when there's an `=` on the right
      Parser(Main).runOrThrow(Array("--foo=bar=qux")) ==>
        "bar=quxbar=qux false"
    }
    test("empty") {
      // --foo= syntax sets `foo` to an empty string
      Parser(Main).runOrThrow(Array("--foo=")) ==>
        " false"
    }
    test("shortName") {
      // -f=bar syntax does work for short names
      Parser(Main).runEither(Array("-f=bar")) ==>
        Right("barbar false")
    }
    test("notFlags") {
      // -f=bar syntax doesn't work for flags
      Parser(Main).runEither(Array("--foo=bar", "--bool=true")) ==>
         Left("""Unknown argument: "--bool=true"
                |Expected Signature: run
                |  --bool          Example flag
                |  -f --foo <str>  String to print repeatedly
                |  --my-num <int>  How many times to print string
                |
                |""".stripMargin)
    }
  }
}
