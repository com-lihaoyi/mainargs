package mainargs
import utest._

object ParserTests extends TestSuite{
  val tests = Tests {
    test("runEitherMulti") {
      test {
        Parser(Array("foo")).runEither[MultiTarget.type] ==> Right(1)
      }
      test {
        Parser(Array("foo")).runEither(MultiTarget) ==> Right(1)
      }
      test {
        Parser(Array("f")).runEither[MultiTarget.type].left.exists(
          _.contains("Unable to find subcommand: f")
        )
      }
      test {
        Parser(Array("f")).runEither(MultiTarget).left.exists(
          _.contains("Unable to find subcommand: f")
        )
      }
    }
    test("runEitherSingle"){
      test {
        Parser(Array("5", "x")).runEither[SingleTarget.type] ==> Right("xxxxx")
      }
      test {
        Parser(Array("5", "x")).runEither(SingleTarget) ==> Right("xxxxx")
      }
    }
    test("constructEither"){
      test {
        Parser(Array("--code", "println(1)")).constructEither[ClassTarget] ==>
          Right(ClassTarget(code = Some("println(1)")))
      }
    }
  }
}
