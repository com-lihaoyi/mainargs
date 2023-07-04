package mainargs
import utest._

object HygieneTests extends TestSuite {

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
    import scala.collection.mutable._
    test("importingSeqShouldntFailCompile") {
      ParserForMethods(Main)
    }

  }
}
