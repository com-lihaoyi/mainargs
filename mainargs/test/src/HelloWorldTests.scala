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
      Parser(Array("--foo", "bar", "--my-num", "3")).runOrThrow(Main) ==>
        "barbarbar false"
    }
    test("short") {
      Parser(Array("-f", "bar")).runOrThrow(Main) ==>
        "barbar false"
    }
    test("default") {
      Parser(Array("--foo", "bar")).runOrThrow(Main) ==>
        "barbar false"
    }
    test("positional") {
      Parser(Array("qux", "4")).runOrThrow(Main) ==>
        "quxquxquxqux false"
    }
    test("flags") {
      Parser(Array("qux", "4", "--bool")).runOrThrow(Main) ==>
        "quxquxquxqux true"
    }
    test("either") {
      Parser(Array("qux", "4", "--bool")).runEither(Main) ==>
        Right("quxquxquxqux true")
    }
  }
}
