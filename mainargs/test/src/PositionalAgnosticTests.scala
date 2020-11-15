package mainargs

import utest._

object PositionalAgnosticEnabledTests extends PositionalAgnosticTests(true)
object PositionalAgnosticDisabledTests extends PositionalAgnosticTests(false)
class PositionalAgnosticTests(allowPositional: Boolean) extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = allowPositional)
      .flatMap(entryPoint.invoke(base, _))
  }
  def check[B, T](base: B,
                  entryPoint: EntryPoint[B],
                  input: List[String],
                  expected: Result[T]) = {
    val result = parseInvoke(base, entryPoint, input)
    assert(result == expected)
  }

  val tests = Tests {
    test("router"){
      val routes = generateRoutes[Target.type].value

      test("formatMainMethods"){
        Renderer.formatMainMethods(Target, routes)
      }
      test("basicModelling") {
        val names = routes.map(_.name)
        assert(
          names == List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic")
        )
        val evaledArgs = routes.map(_.argSigs.map{
          case ArgSig(name, s, tpe, docs, None, _, _) => (name, tpe, docs, None)
          case ArgSig(name, s, tpe, docs, Some(default), _, _) =>
            (name, tpe, docs, Some(default(Target)))
        })
        assert(
          evaledArgs == List(
            List(),
            List(("i", "Int", None, None)),
            List(
              ("i", "Int", None, None),
              ("s", "String", Some("Pass in a custom `s` to override it"), Some("lols"))
            ),
            List(),
            List(("nums", "Int*", None, None)),
            List(("first", "Int", None, None), ("args", "String*", None, None))
          )
        )
      }


      test("invoke"){
        test - check(Target, routes(0), List(), Result.Success(1))
        test - check(Target, routes(1), List("--i", "2"), Result.Success(2))
        test - check(Target, routes(2), List("--i", "2"), Result.Success("lolslols"))
        test - check(Target, routes(2), List("--i", "3", "--s", "x"), Result.Success("xxx"))
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(Target, routes(4), List("1", "2", "3"), Result.Success(6))
        }
        test("emptyVarargsPasses"){
          test - check(Target, routes(4), List(), Result.Success(0))
          test - check(Target, routes(5), List("-f", "1"), Result.Success("1"))
        }
        test("varargsAreAlwaysPositional"){
          val invoked = parseInvoke(Target, routes(4), List("--nums", "31337"))
          test - assertMatch(invoked){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Invalid(
            ArgSig("nums", _, "Int*", _, _, true, _),
            "--nums",
            _: NumberFormatException
            )
            ))=>
          }

          test - assertMatch(parseInvoke(Target, routes(4), List("1", "2", "3", "--nums", "4"))){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Invalid(
            ArgSig("nums", _, "Int*", _, _, true, _),
            "--nums",
            _: NumberFormatException
            )
            ))=>
          }
        }

        test("notEnoughNormalArgsStillFails"){
          assertMatch(parseInvoke(Target, routes(5), List())){
            case Result.Error.MismatchedArguments(List(ArgSig("first", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
        }
        test("multipleVarargParseFailures"){
          test - assertMatch(parseInvoke(Target, routes(4), List("aa", "bb", "3"))){
            case Result.Error.InvalidArguments(
            List(
            Result.ParamError.Invalid(ArgSig("nums", _, "Int*", _, _, true, _), "aa", _: NumberFormatException),
            Result.ParamError.Invalid(ArgSig("nums", _, "Int*", _, _, true, _), "bb", _: NumberFormatException)
            )
            )=>
          }
        }
      }

      test("failures"){
        test("missingParams"){
          test - assertMatch(parseInvoke(Target, routes(1), List.empty)){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
          test - assertMatch(parseInvoke(Target, routes(2), List("--s", "omg"))){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
        }

        test("tooManyParams") - check(
          Target, routes(0), List("1", "2"),
          Result.Error.MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )

        test("failing") - check(
          Target,
          routes(3),
          List(),
          Result.Error.Exception(MyException)
        )
      }
    }
  }
}
