package mainargs

import mainargs.Result.Error.MismatchedArguments
import utest._

object PositionalDisabledTests extends TestSuite{
  val check = new Checker(MultiTarget, allowPositional = false)

  val tests = Tests {
    test("invoke"){
      test - check(
        List("bar", "2"),
        MismatchedArguments(
          missing = List(ArgSig("i",None,"int",None,None,false,false)),
          unknown = List("2")
        )
      )
      test - check(
        List("qux", "2"),
        MismatchedArguments(
          missing = List(ArgSig("i",None,"int",None,None,false,false)),
          unknown = List("2")
        )
      )
      test - check(
        List("qux", "3", "x"),
        MismatchedArguments(
          missing = List(ArgSig("i",None,"int",None,None,false,false)),
          unknown = List("3", "x")
        )
      )
      test - check(
        List("qux", "--i", "3", "x"),
        MismatchedArguments(List(),List("x"),List(),None)
      )
    }
    test("varargs"){
      test("happyPathPasses"){
        test - check(
          List("mixedVariadic", "1", "2", "3", "4", "5"),
          MismatchedArguments(
            missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
          )
        )
      }
      test("emptyVarargsPasses")- check(
        List("mixedVariadic", "1"),
        MismatchedArguments(
          missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
        )
      )
      test("varargsAreAlwaysPositional")- check(
        List("mixedVariadic", "1", "--args", "foo"),
        MismatchedArguments(
          missing = List(ArgSig("first",Some('f'),"int",None,None,false,false))
        )
      )

      test("multipleVarargParseFailures") - check(
        List("mixedVariadic", "aa", "bb", "3"),
        MismatchedArguments(
          missing = List(ArgSig("first",Some('f'),"int",None,None,false,false)),
        )
      )
    }

    test("failures"){
      test("invalidParams") - check(
        List("bar", "lol"),
        MismatchedArguments(
          missing = List(ArgSig("i",None,"int",None,None,false,false)),
          unknown = List("lol"),
        )
      )
    }

    test("redundantParams")- check(
      List("qux", "1", "--i", "2"),
      MismatchedArguments(
        missing = List(ArgSig("i", None,"int",None,None,false,false)),
        unknown = List("1", "--i", "2"),
      )
    )
  }
}
