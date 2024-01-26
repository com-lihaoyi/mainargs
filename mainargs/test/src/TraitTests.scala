package mainargs
import utest._

object TraitTests extends TestSuite {

  trait CommandList {
    @main
    def list(@arg v: String) = v
  }

  trait CommandCopy {
    @main
    def copy(@arg from: String, @arg to: String) = (from, to)
  }

  object Main extends CommandList with CommandCopy {
    def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
  }

  val tests = Tests {
    test("explicit") {
      ParserForMethods(Main).runOrThrow(Array("list", "-v", "hello")) ==> "hello"
      ParserForMethods(Main).runOrThrow(Array("copy", "--from", "hello", "--to", "world")) ==> ("hello", "world")
    }
  }
}
