package mainargs

import utest._

object PositionalEnabledTests extends TestSuite{
  val check = new Checker(allowPositional = true)

  val tests = Tests {
    test("router"){
      val routes = Mains.generate[MultiTarget.type]

      test("invoke"){
        test - check(MultiTarget, routes, List("bar", "2"), Result.Success(2))
        test - check(MultiTarget, routes, List("qux", "2"), Result.Success("lolslols"))
        test - check(MultiTarget, routes, List("qux", "3", "x"), Result.Success("xxx"))
        test - check(
          MultiTarget, routes, List("qux", "--i", "3", "x"), Result.Success("xxx")
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            MultiTarget,
            routes,
            List("mixedVariadic", "1", "2", "3", "4", "5"),
            Result.Success("12345")
          )
        }
        test("emptyVarargsPasses"){
          test - check(
            MultiTarget, routes, List("mixedVariadic", "1"), Result.Success("1")
          )
        }
        test("varargsAreAlwaysPositional"){

          test - check(
            MultiTarget, routes, List("mixedVariadic", "1", "--args", "foo"),
            Result.Success("1--argsfoo")
          )

        }

        test("multipleVarargParseFailures"){
          test - assertMatch(
            check.parseInvoke(MultiTarget, routes, List("mixedVariadic", "aa", "bb", "3"))
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
          check.parseInvoke(MultiTarget, routes, List("bar", "lol"))
        ){
          case Result.Error.InvalidArguments(
          List(Result.ParamError.Failed(ArgSig("i", _, _, _, _, _, _), "lol", _))
          ) =>
        }

        test("redundantParams"){
          val parsed = check.parseInvoke(MultiTarget, routes, List("qux", "1", "--i", "2"))
          assertMatch(parsed){
            case Result.Error.MismatchedArguments(
            Nil, Nil, Seq((ArgSig("i", _, _, _, _, false, _), Seq("1", "2"))), None
            ) =>
          }
        }
      }
    }
  }
}
