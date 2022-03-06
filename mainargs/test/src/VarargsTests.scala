package mainargs
import utest._

trait VarargsTests extends TestSuite {
  def check: Checker[_]
  def isNewVarargsTests: Boolean
  val tests = Tests {

    test("happyPathPasses") {
      test - check(
        List("pureVariadic", "1", "2", "3"),
        Result.Success(6)
      )
      test - check(
        List("mixedVariadic", "1", "2", "3", "4", "5"),
        Result.Success("12345")
      )
      test - {
        if (isNewVarargsTests)
          check(
            List("mixedVariadicWithDefault"),
            Result.Success("1337")
          )
      }
    }
    test("emptyVarargsPasses") {
      test - check(List("pureVariadic"), Result.Success(0))
      test - check(
        List("mixedVariadic", "-f", "1"),
        Result.Success("1")
      )
      test - check(
        List("mixedVariadic", "1"),
        Result.Success("1")
      )
    }
    test("varargsAreAlwaysPositional") {
      val invoked = check.parseInvoke(
        List("pureVariadic", "--nums", "31337")
      )
      test - assertMatch(invoked) {
        case Result.Failure.InvalidArguments(
              List(
                Result.ParamError.Failed(
                  ArgSig.Leftover("nums", _, _),
                  Seq("--nums"),
                  """java.lang.NumberFormatException: For input string: "--nums"""" |
                  """java.lang.NumberFormatException: --nums"""
                )
              )
            ) =>
      }

      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "1", "2", "3", "--nums", "4"))
      ) {
        case Result.Failure.InvalidArguments(
              List(
                Result.ParamError.Failed(
                  ArgSig.Leftover("nums", _, _),
                  Seq("--nums"),
                  "java.lang.NumberFormatException: For input string: \"--nums\"" |
                  "java.lang.NumberFormatException: --nums"
                )
              )
            ) =>
      }
      test - check(
        List("mixedVariadic", "1", "--args", "foo"),
        Result.Success("1--argsfoo")
      )

    }

    test("notEnoughNormalArgsStillFails") {
      assertMatch(check.parseInvoke(List("mixedVariadic"))) {
        case Result.Failure.MismatchedArguments(
              Seq(ArgSig.Simple(Some("first"), _, _, _, _, _)),
              Nil,
              Nil,
              None
            ) =>
      }
    }
    test("multipleVarargParseFailures") {
      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "aa", "bb", "3"))
      ) {
        case Result.Failure.InvalidArguments(
              List(
                Result.ParamError.Failed(
                  ArgSig.Leftover("nums", _, _),
                  Seq("aa"),
                  "java.lang.NumberFormatException: For input string: \"aa\"" |
                  "java.lang.NumberFormatException: aa"
                ),
                Result.ParamError.Failed(
                  ArgSig.Leftover("nums", _, _),
                  Seq("bb"),
                  "java.lang.NumberFormatException: For input string: \"bb\"" |
                  "java.lang.NumberFormatException: bb"
                )
              )
            ) =>
      }

      test - assertMatch(
        check.parseInvoke(List("mixedVariadic", "aa", "bb", "3"))
      ) {
        case Result.Failure.InvalidArguments(
              List(
                Result.ParamError.Failed(
                  ArgSig.Simple(Some("first"), _, _, _, _, _),
                  Seq("aa"),
                  "java.lang.NumberFormatException: For input string: \"aa\"" |
                  "java.lang.NumberFormatException: aa"
                )
              )
            ) =>
      }
    }
  }
}
