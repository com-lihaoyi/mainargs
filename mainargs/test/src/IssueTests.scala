package mainargs
import utest._

object IssueTests extends TestSuite {

  object Main {
    @main
    def mycmd(
        @arg(name = "the-flag") f: mainargs.Flag = mainargs.Flag(false),
        @arg str: String = "s",
        args: Leftover[String]
    ) = {
      (f.value, str, args.value)
    }
  }

  val tests = Tests {
    test("issue60") {
      test {
        val parsed = ParserForMethods(Main)
          .runEither(Seq("--str", "str", "a", "b", "c", "d"), allowPositional = true)

        assert(parsed == Right((false, "str", List("a", "b", "c", "d"))))
      }
      test {
        val parsed = ParserForMethods(Main)
          .runEither(Seq("a", "b", "c", "d"), allowPositional = true)

        assert(parsed == Right((false, "a", List("b", "c", "d"))))
      }
    }
  }
}
