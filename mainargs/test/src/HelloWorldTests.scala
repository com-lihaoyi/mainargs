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
      Parser(Main).runOrThrow(Array("--foo", "bar", "--my-num", "3")) ==>
        "barbarbar false"
    }
    test("short") {
      Parser(Main).runOrThrow(Array("-f", "bar")) ==>
        "barbar false"
    }
    test("default") {
      Parser(Main).runOrThrow(Array("--foo", "bar")) ==>
        "barbar false"
    }
    test("positional") {
      Parser(Main).runOrThrow(Array("qux", "4"), allowPositional = true) ==>
        "quxquxquxqux false"
    }
    test("flags") {
      Parser(Main).runOrThrow(Array("qux", "4", "--bool"), allowPositional = true) ==>
        "quxquxquxqux true"
    }
    test("either") {
      Parser(Main).runEither(Array("qux", "4", "--bool"), allowPositional = true) ==>
        Right("quxquxquxqux true")
    }
  }
}
