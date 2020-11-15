package mainargs

import utest._

object PositionalEnabledTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = true)
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

      test("invoke"){
        test - check(MultiTarget, routes("bar"), List("2"), Result.Success(2))
        test - check(MultiTarget, routes("qux"), List("2"), Result.Success("lolslols"))
        test - check(MultiTarget, routes("qux"), List("3", "x"), Result.Success("xxx"))
        test - check(
          MultiTarget, routes("qux"), List("--i", "3", "x"), Result.Success("xxx")
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            MultiTarget,
            routes("mixedVariadic"),
            List("1", "2", "3", "4", "5"),
            Result.Success("12345")
          )
        }
        test("emptyVarargsPasses"){
          test - check(
            MultiTarget, routes("mixedVariadic"), List("1"), Result.Success("1")
          )
        }
        test("varargsAreAlwaysPositional"){

          test - check(
            MultiTarget, routes("mixedVariadic"), List("1", "--args", "foo"),
            Result.Success("1--argsfoo")
          )

        }

        test("multipleVarargParseFailures"){
          test - assertMatch(
            parseInvoke(MultiTarget, routes("mixedVariadic"), List("aa", "bb", "3"))
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
          parseInvoke(MultiTarget, routes("bar"), List("lol"))
        ){
          case Result.Error.InvalidArguments(
          List(Result.ParamError.Failed(ArgSig("i", _, _, _, _, _, _), "lol", _))
          ) =>
        }

        test("redundantParams"){
          val parsed = parseInvoke(MultiTarget, routes("qux"), List("1", "--i", "2"))
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

