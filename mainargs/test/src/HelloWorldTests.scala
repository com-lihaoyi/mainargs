package mainargs
import utest._


object HelloWorldTests extends TestSuite{

  object Main{
    @main
    def run(@arg(short = 'f', doc = "String to print repeatedly")
            foo: String,
            @arg(name = "my-num", doc = "How many times to print string")
            myNum: Int = 2,
            @arg(flag = true, doc = "Example flag")
            bool: Boolean) = {
      foo * myNum + " " + bool
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
      ParserForMethods(Main).runOrThrow(Array("--foo", "bar"))  ==>
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
  }
}
