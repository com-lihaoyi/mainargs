package mainargs

import mainargs.Result.Error.MismatchedArguments
import utest._

object PositionalDisabledTests extends TestSuite{
  val check = new Checker(allowPositional = false)

  val tests = Tests {
    test("router"){

      val routes = generateMains[MultiTarget.type]

      test("invoke"){
        test - check(
          MultiTarget, routes, List("bar", "2"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"int",None,None,false,false)),
            unknown = List("2")
          )
        )
        test - check(
          MultiTarget, routes, List("qux", "2"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"int",None,None,false,false)),
            unknown = List("2")
          )
        )
        test - check(
          MultiTarget, routes, List("qux", "3", "x"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"int",None,None,false,false)),
            unknown = List("3", "x")
          )
        )
        test - check(
          MultiTarget, routes, List("qux", "--i", "3", "x"),
          MismatchedArguments(List(),List("x"),List(),None)
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            MultiTarget, routes, List("mixedVariadic", "1", "2", "3", "4", "5"),
            MismatchedArguments(
              missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
            )
          )
        }
        test("emptyVarargsPasses")- check(
          MultiTarget, routes, List("mixedVariadic", "1"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
          )
        )
        test("varargsAreAlwaysPositional")- check(
          MultiTarget, routes, List("mixedVariadic", "1", "--args", "foo"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
          )
        )

        test("multipleVarargParseFailures") - check(
          MultiTarget, routes, List("mixedVariadic", "aa", "bb", "3"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"int",None,None,false,false)),
          )
        )
      }

      test("failures"){
        test("invalidParams") - check(
          MultiTarget, routes, List("bar", "lol"),
          MismatchedArguments(
            missing = List(ArgSig("i",None,"int",None,None,false,false)),
            unknown = List("lol"),
          )
        )
      }

      test("redundantParams")- check(
        MultiTarget,
        routes,
        List("qux", "1", "--i", "2"),
        MismatchedArguments(
          missing = List(ArgSig("i", None,"int",None,None,false,false)),
          unknown = List("1", "--i", "2"),
        )
      )
    }
  }
}
