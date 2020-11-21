package mainargs
import utest._



object VarargsTests extends TestSuite{
  object Base{
    @main
    def pureVariadic(nums: LeftoverTokens[Int]) = nums.value.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: LeftoverTokens[String]) = first + args.value.mkString
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
          LeftoverArgSig("nums", _, _),
          Seq("--nums"),
          """java.lang.NumberFormatException: For input string: "--nums""""
        )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "1", "2", "3", "--nums", "4"))
      ){
        case Result.Error.InvalidArguments(List(
          Result.ParamError.Failed(
          LeftoverArgSig("nums", _, _),
          Seq("--nums"),
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
          Seq(ArgSig("first", _, _, _, _, _)),
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
          LeftoverArgSig("nums", _, _),
            Seq("aa"),
            "java.lang.NumberFormatException: For input string: \"aa\""
          ),
          Result.ParamError.Failed(
          LeftoverArgSig("nums", _, _),
            Seq("bb"),
            "java.lang.NumberFormatException: For input string: \"bb\""
          )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("mixedVariadic", "aa", "bb", "3"))
      ){
        case Result.Error.InvalidArguments(List(
          Result.ParamError.Failed(
            ArgSig("first", _, _, _, _, _),
            Seq("aa"),
            "java.lang.NumberFormatException: For input string: \"aa\""
          )
        ))=>
      }
    }
  }
}
