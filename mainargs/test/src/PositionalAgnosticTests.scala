package mainargs

import utest._

object PositionalAgnosticEnabledTests extends PositionalAgnosticTests(true)
object PositionalAgnosticDisabledTests extends PositionalAgnosticTests(false)

class PositionalAgnosticTests(allowPositional: Boolean) extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = allowPositional)
      .flatMap(entryPoint.invoke(base, _))
      .map(_.value)
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
      val routes0 = generateRoutes[MultiTarget.type].value
      val routes = routes0.map(x => (x.name, x)).toMap

      test("formatMainMethods"){
        Renderer.formatMainMethods(MultiTarget, routes0, 95)
      }
      test("basicModelling") {
        val names = routes0.map(_.name)
        assert(
          names == List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic", "flaggy")
        )
        val evaledArgs = routes0.map(_.argSigs.map{
          case ArgSig(name, s, tpe, docs, None, _, _) => (name, tpe, docs, None)
          case ArgSig(name, s, tpe, docs, Some(default), _, _) =>
            (name, tpe, docs, Some(default(MultiTarget)))
        })

        assert(
          evaledArgs == List(
            List(),
            List(("i", "int", None, None)),
            List(
              ("i", "int", None, None),
              ("s", "str", Some("Pass in a custom `s` to override it"), Some("lols"))
            ),
            List(),
            List(("nums", "int", None, None)),
            List(("first", "int", None, None), ("args", "str", None, None)),
            List(
              ("a", "bool", None, Some(false)),
              ("b", "bool", None, None),
              ("c", "bool", None, Some(false))
            )
          )
        )
      }

      test("invoke"){
        test - check(MultiTarget, routes("foo"), List(), Result.Success(1))
        test - check(MultiTarget, routes("bar"), List("--i", "2"), Result.Success(2))
        test - check(MultiTarget, routes("qux"), List("--i", "2"), Result.Success("lolslols"))
        test - check(MultiTarget, routes("qux"), List("--i", "3", "--s", "x"), Result.Success("xxx"))
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(MultiTarget, routes("pureVariadic"), List("1", "2", "3"), Result.Success(6))
        }
        test("emptyVarargsPasses"){
          test - check(MultiTarget, routes("pureVariadic"), List(), Result.Success(0))
          test - check(MultiTarget, routes("mixedVariadic"), List("-f", "1"), Result.Success("1"))
        }
        test("varargsAreAlwaysPositional"){
          val invoked = parseInvoke(MultiTarget, routes("pureVariadic"), List("--nums", "31337"))
          test - assertMatch(invoked){
            case Result.Error.InvalidArguments(List(
              Result.ParamError.Failed(
              ArgSig("nums", _, "int", _, _, true, _),
              "--nums",
              """java.lang.NumberFormatException: For input string: "--nums""""
            )
            ))=>
          }

          test - assertMatch(parseInvoke(MultiTarget, routes("pureVariadic"), List("1", "2", "3", "--nums", "4"))){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Failed(
            ArgSig("nums", _, "int", _, _, true, _),
            "--nums",
            "java.lang.NumberFormatException: For input string: \"--nums\""
            )
            ))=>
          }
        }

        test("notEnoughNormalArgsStillFails"){
          assertMatch(parseInvoke(MultiTarget, routes("mixedVariadic"), List())){
            case Result.Error.MismatchedArguments(List(ArgSig("first", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
        }
        test("multipleVarargParseFailures"){
          test - assertMatch(parseInvoke(MultiTarget, routes("pureVariadic"), List("aa", "bb", "3"))){
            case Result.Error.InvalidArguments(
            List(
            Result.ParamError.Failed(ArgSig("nums", _, "int", _, _, true, _), "aa", "java.lang.NumberFormatException: For input string: \"aa\""),
            Result.ParamError.Failed(ArgSig("nums", _, "int", _, _, true, _), "bb", "java.lang.NumberFormatException: For input string: \"bb\"")
            )
            )=>
          }
        }
      }


      test("flags"){
        test - check(MultiTarget, routes("flaggy"), List("--b", "true"), Result.Success(true))
        test - check(MultiTarget, routes("flaggy"), List("--b", "false"), Result.Success(false))
        test - check(MultiTarget, routes("flaggy"), List("--a", "--b", "false"), Result.Success(true))
        test - check(MultiTarget, routes("flaggy"), List("--c", "--b", "false"), Result.Success(true))
        test - check(MultiTarget, routes("flaggy"), List("--a", "--c", "--b", "false"), Result.Success(true))
      }

      test("failures"){
        test("missingParams"){
          test - assertMatch(parseInvoke(MultiTarget, routes("bar"), List.empty)){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
          test - assertMatch(parseInvoke(MultiTarget, routes("qux"), List("--s", "omg"))){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false, _)), Nil, Nil, None) =>
          }
        }

        test("tooManyParams") - check(
          MultiTarget, routes("foo"), List("1", "2"),
          Result.Error.MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )

        test("failing") - check(
          MultiTarget,
          routes("ex"),
          List(),
          Result.Error.Exception(MyException)
        )
      }
    }
  }
}

