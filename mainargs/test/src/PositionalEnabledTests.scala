package mainargs

import utest._

object PositionalEnabledTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = true)
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

      test("invoke"){
        test - check(Target, routes(1), List("2"), Result.Success(2))
        test - check(Target, routes(2), List("2"), Result.Success("lolslols"))
        test - check(Target, routes(2), List("3", "x"), Result.Success("xxx"))
        test - check(Target, routes(2), List("--i", "3", "x"), Result.Success("xxx"))
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(Target, routes(5), List("1", "2", "3", "4", "5"), Result.Success("12345"))
        }
        test("emptyVarargsPasses"){
          test - check(Target, routes(5), List("1"), Result.Success("1"))
        }
        test("varargsAreAlwaysPositional"){

          test - check(
            Target, routes(5), List("1", "--args", "foo"),
            Result.Success("1--argsfoo")
          )

        }

        test("multipleVarargParseFailures"){
          test - assertMatch(parseInvoke(Target, routes(5), List("aa", "bb", "3"))){
            case Result.Error.InvalidArguments(
            List(
            Result.ParamError.Invalid(ArgSig("first", _, "Int", _, _, fals, _), "aa", _: NumberFormatException)
            )
            )=>
          }
        }
      }

      test("failures"){
        test("invalidParams") - assertMatch(parseInvoke(Target, routes(1), List("lol"))){
          case Result.Error.InvalidArguments(
          List(Result.ParamError.Invalid(ArgSig("i", _, _, _, _, _, _), "lol", _))
          ) =>
        }



        test("redundantParams"){
          val parsed = parseInvoke(Target, routes(2), List("1", "--i", "2"))
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
