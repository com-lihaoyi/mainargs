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

  val multiMethodParser = ParserForMethods(MultiBase)
  val singleMethodParser = ParserForMethods(SingleBase)
  val classParser = ParserForClass[ClassBase]
  val tests = Tests {
    test("runEitherMulti") {

      test {
        multiMethodParser.runEither(Array("foo")) ==> Right(1)
      }
      test {
        multiMethodParser.runEither(Array("bar", "-i", "123")) ==> Right(123)
      }
      test {
        assert(
          multiMethodParser.runEither(Array("f"))
          .left
          .exists(_.contains("Unable to find subcommand: f"))
        )
      }
    }
    test("runEitherSingle"){
      singleMethodParser.runEither(Array("5", "x"), allowPositional = true) ==> Right("xxxxx")
    }
    test("constructEither"){
      classParser.constructEither(Array("--code", "println(1)")) ==>
        Right(ClassBase(code = Some("println(1)"), other = "hello"))
    }
  }
}
