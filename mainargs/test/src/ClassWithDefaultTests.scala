package mainargs
import utest._

// Make sure
object ClassWithDefaultTests extends TestSuite {
  @main
  case class Foo(x: Int, y: Int = 1)

  implicit val fooParser: ParserForClass[Foo] = ParserForClass[Foo]

  object Main {
    @main
    def run(foo: Foo, bool: Boolean = false) = s"${foo.x} ${foo.y} $bool"
  }

  val mainParser = ParserForMethods(Main)

  val tests = Tests {
    test("simple") {
      test("success") {
        fooParser.constructOrThrow(Seq("-x", "1", "-y", "2")) ==> Foo(1, 2)
      }
      test("default") {
        fooParser.constructOrThrow(Seq("-x", "0")) ==> Foo(0, 1)
      }
      test("missing") {
        fooParser.constructRaw(Seq()) ==>
          Result.Failure.MismatchedArguments(
            Seq(
              ArgSig(
                None,
                Some('x'),
                None,
                None,
                mainargs.TokensReader.IntRead,
                positional = false,
                hidden = false
              )
            ),
            List(),
            List(),
            None
          )

      }
    }

    test("nested") {
      test("success") {
        mainParser.runOrThrow(Seq("-x", "1", "-y", "2", "--bool", "true")) ==> "1 2 true"
      }
      test("default") {
        mainParser.runOrThrow(Seq("-x", "1", "-y", "2")) ==> "1 2 false"
      }
      test("default2") {
        mainParser.runOrThrow(Seq("-x", "0")) ==> "0 1 false"
      }
    }
  }
}
