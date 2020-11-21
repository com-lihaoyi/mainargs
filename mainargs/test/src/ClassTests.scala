package mainargs
import utest._


object ClassTests extends TestSuite{

  @main
  case class Foo(x: Int, y: Int)

  @main
  case class Bar(@arg(flag = true) w: Boolean = false, f: Foo, @arg(short = 'z') zzzz: String)

  @main
  case class Qux(moo: String, b: Bar)

  implicit val fooParser = ParserForClass[Foo]
  implicit val barParser = ParserForClass[Bar]
  implicit val quxParser = ParserForClass[Qux]

  object Main{
    @main
    def run(bar: Bar,
            bool: Boolean = false) = {
      bar.w + " " + bar.f.x + " " + bar.f.y + " " + bar.zzzz + " " + bool
    }
  }

  val tests = Tests {
    test("simple") {
      test("success"){
        fooParser.constructOrThrow(Seq("--x", "1", "--y", "2")) ==> Foo(1, 2)
      }
      test("missing") {
        fooParser.constructRaw(Seq("--x", "1")) ==>
          Result.Error.MismatchedArguments(
            Seq(ArgSig.Simple("y",None,None,None,false,mainargs.TokensReader.IntRead)),
            List(),
            List(),
            None
          )

      }
    }

    test("nested") {
      test("success"){
        barParser.constructOrThrow(Seq("--w", "--x", "1", "--y", "2", "--zzzz", "xxx")) ==>
          Bar(true, Foo(1, 2), "xxx")
      }
      test("missingInner"){
        barParser.constructRaw(Seq("--w", "--x", "1", "--z", "xxx")) ==>
          Result.Error.MismatchedArguments(
            Seq(ArgSig.Simple("y",None,None,None,false,mainargs.TokensReader.IntRead)),
            List(),
            List(),
            None
          )
      }
      test("missingOuter"){
        barParser.constructRaw(Seq("--w", "--x", "1", "--y", "2")) ==>
          Result.Error.MismatchedArguments(
            Seq(ArgSig.Simple("zzzz",Some('z'),None,None,false,mainargs.TokensReader.StringRead)),
            List(),
            List(),
            None
          )
      }

      test("missingInnerOuter"){
        barParser.constructRaw(Seq("--w", "--x", "1")) ==>
          Result.Error.MismatchedArguments(
            Seq(
              ArgSig.Simple("y",None,None,None,false,mainargs.TokensReader.IntRead),
              ArgSig.Simple("zzzz",Some('z'),None,None,false,mainargs.TokensReader.StringRead)
            ),
            List(),
            List(),
            None
          )
      }
      test("failedInnerOuter") {
        assertMatch(barParser.constructRaw(Seq("--w","--x", "xxx", "--y", "hohoho", "-z", "xxx"))) {
          case Result.Error.InvalidArguments(
            Seq(
              Result.ParamError.Failed(ArgSig.Simple("x", None, None, None, false, _), Seq("xxx"), _),
              Result.ParamError.Failed(ArgSig.Simple("y", None, None, None, false, _), Seq("hohoho"), _)
          )
          ) =>
        }
      }
    }

    test("doubleNested"){
      quxParser.constructOrThrow(Seq("--w", "--x", "1", "--y", "2", "--z", "xxx", "--moo", "cow")) ==>
        Qux("cow", Bar(true, Foo(1, 2), "xxx"))
    }
    test("success"){
      ParserForMethods(Main).runOrThrow(Seq("--x", "1", "--y", "2", "-z", "hello")) ==> "false 1 2 hello false"
    }
  }
}

