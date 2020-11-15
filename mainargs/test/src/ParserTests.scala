package mainargs
import utest._


object ParserTests extends TestSuite{

  object SingleBase{
    @main(doc = "Qux is a function that does stuff")
    def run(i: Int,
            @arg(doc = "Pass in a custom `s` to override it")
            s: String  = "lols") = s * i
  }

  object MultiBase{
    @main
    def foo() = 1

    @main
    def bar(i: Int) = i
  }

  @main
  case class ClassBase(code: Option[String] = None, other: String = "hello")

  val tests = Tests {
    test("runEitherMulti") {
      test {
        Parser(Array("foo")).runEither[MultiBase.type] ==> Right(1)
      }
      test {
        Parser(Array("foo")).runEither(MultiBase) ==> Right(1)
      }
      test {
        Parser(Array("bar", "--i", "123")).runEither(MultiBase) ==> Right(123)
      }
      test {
        Parser(Array("f")).runEither[MultiBase.type].left.exists(
          _.contains("Unable to find subcommand: f")
        )
      }
      test {
        Parser(Array("f")).runEither(MultiBase).left.exists(
          _.contains("Unable to find subcommand: f")
        )
      }
    }
    test("runEitherSingle"){
      test {
        Parser(Array("5", "x")).runEither[SingleBase.type] ==> Right("xxxxx")
      }
      test {
        Parser(Array("5", "x")).runEither(SingleBase) ==> Right("xxxxx")
      }
    }
    test("constructEither"){
      test {
        Parser(Array("--code", "println(1)")).constructEither[ClassBase] ==>
          Right(ClassBase(code = Some("println(1)"), other = "hello"))
      }
    }
  }
}
