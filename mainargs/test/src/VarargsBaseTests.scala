package mainargs
import utest._

trait VarargsBaseTests extends TestSuite {
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
                  ArgSig(Some("nums"), _, _, _, _, _, _),
                  Seq("--nums", "31337"),
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
                  ArgSig(Some("nums"), _, _, _, _, _, _),
                  Seq("1", "2", "3", "--nums", "4"),
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
              Seq(ArgSig(Some("first"), _, _, _, _, _, _)),
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
                  ArgSig(Some("nums"), _, _, _, _, _, _),
                  Seq("aa", "bb", "3"),
                  "java.lang.NumberFormatException: For input string: \"aa\"" |
                  "java.lang.NumberFormatException: aa"
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
                  ArgSig(Some("first"), _, _, _, _, _, _),
                  Seq("aa"),
                  "java.lang.NumberFormatException: For input string: \"aa\"" |
                  "java.lang.NumberFormatException: aa"
                )
              )
            ) =>
      }
    }

    test("failedCombinedShortArgsGoToLeftover") {
      test - check(
        List("mixedVariadic", "-f", "123", "abc", "xyz"),
        Result.Success("123abcxyz")
      )
      test - check(
        List("mixedVariadic", "-f123", "456", "abc", "xyz"),
        Result.Success("123456abcxyz")
      )
      test - check(
        List("mixedVariadic", "-f123", "-unknown", "456", "abc", "xyz"),
        Result.Success("123-unknown456abcxyz")
      )
    }
  }
}
