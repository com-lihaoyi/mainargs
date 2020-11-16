package mainargs
import utest._


object HelloWorldTests extends TestSuite{
  object Main{
    @main
    def run(@arg(short = 'f') foo: String,
            @arg(name = "my-num") myNum: Int = 2,
            @arg(flag = true) bool: Boolean = false) = {
      foo * myNum + " " + bool
    }
  }

  val tests = Tests {
    test("explicit") {
      ParserForMethods(Main).runOrThrow(Array("--foo", "bar", "--my-num", "3"))==>
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
      ParserForMethods(Main).runOrThrow(Array("qux", "4")) ==>
        "quxquxquxqux false"
    }
    test("flags") {
      ParserForMethods(Main).runOrThrow(Array("qux", "4", "--bool")) ==>
        "quxquxquxqux true"
    }
    test("either") {
      ParserForMethods(Main).runEither(Array("qux", "4", "--bool")) ==>
        Right("quxquxquxqux true")
    }
  }
}
