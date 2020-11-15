package mainargs

import mainargs.Result.Error.{InvalidArguments, MismatchedArguments}
import utest._

object PositionalDisabledTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = false)
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
        test - check(
          Target, routes(1), List("2"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"Int",None,None,false,false)),
            unknown = List("2")
          )
        )
        test - check(
          Target, routes(2), List("2"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"Int",None,None,false,false)),
            unknown = List("2")
          )
        )
        test - check(
          Target, routes(2), List("3", "x"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"Int",None,None,false,false)),
            unknown = List("3", "x")
          )
        )
        test - check(
          Target, routes(2), List("--i", "3", "x"),
          MismatchedArguments(List(),List("x"),List(),None)
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            Target, routes(5), List("1", "2", "3", "4", "5"),
            MismatchedArguments(
              missing = List(ArgSig("first",Some('f'),"Int",None,None,false,false))
            )
          )
        }
        test("emptyVarargsPasses")- check(
          Target, routes(5), List("1"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"Int",None,None,false,false))
          )
        )
        test("varargsAreAlwaysPositional")- check(
          Target, routes(5), List("1", "--args", "foo"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"Int",None,None,false,false))
          )
        )

        test("multipleVarargParseFailures") - check(
          Target, routes(5), List("aa", "bb", "3"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"Int",None,None,false,false)),
          )
        )
      }

      test("failures"){
        test("invalidParams") - check(
          Target, routes(1), List("lol"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"Int",None,None,false,false)),
            unknown = List("lol"),
          )
        )
      }

      test("redundantParams")- check(
        Target,
        routes(2),
        List("1", "--i", "2"),
        MismatchedArguments(
          missing = List(ArgSig("i", None,"Int",None,None,false,false)),
          unknown = List("1", "--i", "2"),
        )
      )
    }
  }
}
