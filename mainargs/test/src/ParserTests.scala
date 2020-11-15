package mainargs
import utest._


object ParserTests extends TestSuite{

  val tests = Tests {
    test("router"){

      test {
        Parser(Array("foo")).runEither[MultiTarget.type] ==> Right(1)
      }
      test {
        Parser(Array("f")).runEither[MultiTarget.type].left.exists(
          _.contains("Unable to find subcommand: f")
        )
      }
      test {
        Parser(Array("5", "x")).runEither[SingleTarget.type] ==> Right("xxxxx")
      }

      test {
        val i = generateClassRoute[ClassTarget]
        Parser(Array("--code", "println(1)")).constructEither[ClassTarget](i) ==>
          Right(ClassTarget(code = Some("println(1)")))
      }
    }
  }
}
