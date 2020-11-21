package mainargs
import utest._



object VarargsTests extends TestSuite{
  object Base{
    @main
    def pureVariadic(nums: Int*) = nums.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: String*) = first + args.mkString
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {

    test("happyPathPasses"){
      test - check(
        List("pureVariadic", "1", "2", "3"), Result.Success(6)
      )
      test - check(
        List("mixedVariadic", "1", "2", "3", "4", "5"),
        Result.Success("12345")
      )
    }
    test("emptyVarargsPasses"){
      test - check(List("pureVariadic"), Result.Success(0))
      test - check(
        List("mixedVariadic", "-f", "1"), Result.Success("1")
      )
      test - check(
        List("mixedVariadic", "1"), Result.Success("1")
      )
    }
    test("varargsAreAlwaysPositional"){
      val invoked = check.parseInvoke(
        List("pureVariadic", "--nums", "31337")
      )
      test - assertMatch(invoked){
        case Result.Error.InvalidArguments(List(
          Result.ParamError.Failed(
          ArgSig("nums", _, _, _, true, _, ArgParser.IntRead),
          "--nums",
          """java.lang.NumberFormatException: For input string: "--nums""""
        )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "1", "2", "3", "--nums", "4"))
      ){
        case Result.Error.InvalidArguments(List(
        Result.ParamError.Failed(
        ArgSig("nums", _, _, _, true, _, ArgParser.IntRead),
        "--nums",
        "java.lang.NumberFormatException: For input string: \"--nums\""
        )
        ))=>
      }
      test - check(
        List("mixedVariadic", "1", "--args", "foo"),
        Result.Success("1--argsfoo")
      )

    }

    test("notEnoughNormalArgsStillFails"){
      assertMatch(check.parseInvoke(List("mixedVariadic"))){
        case Result.Error.MismatchedArguments(
          List(ArgSig("first", _, _, _, false, _, ArgParser.IntRead)),
          Nil,
          Nil,
          None
        ) =>
      }
    }
    test("multipleVarargParseFailures"){
      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "aa", "bb", "3"))
      ){
        case Result.Error.InvalidArguments(List(
          Result.ParamError.Failed(
            ArgSig("nums", _, _, _, true, _, ArgParser.IntRead),
            "aa",
            "java.lang.NumberFormatException: For input string: \"aa\""
          ),
          Result.ParamError.Failed(
            ArgSig("nums", _, _, _, true, _, ArgParser.IntRead),
            "bb",
            "java.lang.NumberFormatException: For input string: \"bb\""
          )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("mixedVariadic", "aa", "bb", "3"))
      ){
        case Result.Error.InvalidArguments(List(
          Result.ParamError.Failed(
            ArgSig("first", _, _, _, fals, _, ArgParser.IntRead),
            "aa",
            "java.lang.NumberFormatException: For input string: \"aa\""
          )
        ))=>
      }
    }
  }
}
