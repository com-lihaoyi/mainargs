package mainargs
import utest._

object ClassTests extends TestSuite {

  @main
  case class Foo(x: Int, y: Int)

  @main
  case class Bar(w: Flag = Flag(), f: Foo, @arg(short = 'z') zzzz: String)

  @main
  case class Qux(moo: String, b: Bar)

  implicit val fooParser: ParserForClass[Foo] = ParserForClass[Foo]
  implicit val barParser: ParserForClass[Bar] = ParserForClass[Bar]
  implicit val quxParser: ParserForClass[Qux] = ParserForClass[Qux]

  object Main {
    @main
    def run(bar: Bar, bool: Boolean = false) = {
      s"${bar.w.value} ${bar.f.x} ${bar.f.y} ${bar.zzzz} $bool"
    }
  }

  val tests = Tests {
    test("simple") {
      test("success") {
        fooParser.constructOrThrow(Seq("-x", "1", "-y", "2")) ==> Foo(1, 2)
      }
      test("missing") {
        fooParser.constructRaw(Seq("-x", "1")) ==>
          Result.Failure.MismatchedArguments(
            Seq(
              ArgSig(
                None,
                Some('y'),
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
        barParser.constructOrThrow(
          Seq("-w", "-x", "1", "-y", "2", "--zzzz", "xxx")
        ) ==>
          Bar(Flag(true), Foo(1, 2), "xxx")
      }
      test("missingInner") {
        barParser.constructRaw(Seq("-w", "-x", "1", "-z", "xxx")) ==>
          Result.Failure.MismatchedArguments(
            Seq(
              ArgSig(
                None,
                Some('y'),
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
      test("missingOuter") {
        barParser.constructRaw(Seq("-w", "-x", "1", "-y", "2")) ==>
          Result.Failure.MismatchedArguments(
            Seq(
              ArgSig(
                Some("zzzz"),
                Some('z'),
                None,
                None,
                mainargs.TokensReader.StringRead,
                positional = false,
                hidden = false
              )
            ),
            List(),
            List(),
            None
          )
      }

      test("missingInnerOuter") {
        barParser.constructRaw(Seq("-w", "-x", "1")) ==>
          Result.Failure.MismatchedArguments(
            Seq(
              ArgSig(
                None,
                Some('y'),
                None,
                None,
                mainargs.TokensReader.IntRead,
                positional = false,
                hidden = false
              ),
              ArgSig(
                Some("zzzz"),
                Some('z'),
                None,
                None,
                mainargs.TokensReader.StringRead,
                positional = false,
                hidden = false
              )
            ),
            List(),
            List(),
            None
          )
      }

      test("failedInnerOuter") {
        assertMatch(
          barParser.constructRaw(
            Seq("-w", "-x", "xxx", "-y", "hohoho", "-z", "xxx")
          )
        ) {
          case Result.Failure.InvalidArguments(
                Seq(
                  Result.ParamError.Failed(
                    ArgSig(None, Some('x'), None, None, _, false, _),
                    Seq("xxx"),
                    _
                  ),
                  Result.ParamError.Failed(
                    ArgSig(None, Some('y'), None, None, _, false, _),
                    Seq("hohoho"),
                    _
                  )
                )
              ) =>

        }
      }
    }

    test("doubleNested") {
      quxParser.constructOrThrow(
        Seq("-w", "-x", "1", "-y", "2", "-z", "xxx", "--moo", "cow")
      ) ==>
        Qux("cow", Bar(Flag(true), Foo(1, 2), "xxx"))
    }
    test("success") {
      ParserForMethods(Main).runOrThrow(
        Seq("-x", "1", "-y", "2", "-z", "hello")
      ) ==> "false 1 2 hello false"
    }
  }
}
