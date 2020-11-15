package mainargs

import utest._

object PositionalEnabledTests extends TestSuite{
  val check = new Checker(MultiTarget, allowPositional = true)

  val tests = Tests {
    test("invoke"){
      test - check(List("bar", "2"), Result.Success(2))
      test - check(List("qux", "2"), Result.Success("lolslols"))
      test - check(List("qux", "3", "x"), Result.Success("xxx"))
      test - check(
        List("qux", "--i", "3", "x"), Result.Success("xxx")
      )
    }
    test("varargs"){
      test("happyPathPasses"){
        test - check(
          List("mixedVariadic", "1", "2", "3", "4", "5"),
          Result.Success("12345")
        )
      }
      test("emptyVarargsPasses"){
        test - check(
          List("mixedVariadic", "1"), Result.Success("1")
        )
      }
      test("varargsAreAlwaysPositional"){
        test - check(
          List("mixedVariadic", "1", "--args", "foo"),
          Result.Success("1--argsfoo")
        )
      }

      test("multipleVarargParseFailures"){
        test - assertMatch(
          check.parseInvoke(List("mixedVariadic", "aa", "bb", "3"))
        ){
          case Result.Error.InvalidArguments(
          List(
            Result.ParamError.Failed(
              ArgSig("first", _, "int", _, _, fals, _),
              "aa",
              "java.lang.NumberFormatException: For input string: \"aa\""
              )
            )
          )=>
        }
      }
    }

    test("failures"){
      test("invalidParams") - assertMatch(
        check.parseInvoke(List("bar", "lol"))
      ){
        case Result.Error.InvalidArguments(
        List(Result.ParamError.Failed(ArgSig("i", _, _, _, _, _, _), "lol", _))
        ) =>
      }

      test("redundantParams"){
        val parsed = check.parseInvoke(List("qux", "1", "--i", "2"))
        assertMatch(parsed){
          case Result.Error.MismatchedArguments(
          Nil, Nil, Seq((ArgSig("i", _, _, _, _, false, _), Seq("1", "2"))), None
          ) =>
        }
      }
    }
  }
}
