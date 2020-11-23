package mainargs
import utest._
object NewVarargsTests extends VarargsTests{
  object Base{
    @main
    def pureVariadic(nums: Leftover[Int]) = nums.value.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: Leftover[String]) = {
      first + args.value.mkString
    }
    @main
    def mixedVariadicWithDefault(@arg(short = 'f') first: Int = 1337,
                                 args: Leftover[String]) = {
      first + args.value.mkString
    }
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
}

object OldVarargsTests extends VarargsTests{
  object Base{

    @main
    def pureVariadic(nums: Int*) = nums.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: String*) = first + args.mkString
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
}

trait VarargsTests extends TestSuite{
  def check: Checker[_]
  val tests = Tests {

    test("happyPathPasses"){
      test - check(
        List("pureVariadic", "1", "2", "3"), Result.Success(6)
      )
      test - check(
        List("mixedVariadic", "1", "2", "3", "4", "5"),
        Result.Success("12345")
      )
      test - {
        if (this == NewVarargsTests) check(
          List("mixedVariadicWithDefault"),
          Result.Success("1337")
        )
      }
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
        case Result.Failure.InvalidArguments(List(
          Result.ParamError.Failed(
          ArgSig.Leftover("nums", _, _),
          Seq("--nums"),
          """java.lang.NumberFormatException: For input string: "--nums""""
        )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("pureVariadic", "1", "2", "3", "--nums", "4"))
      ){
        case Result.Failure.InvalidArguments(List(
          Result.ParamError.Failed(
          ArgSig.Leftover("nums", _, _),
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
        case Result.Failure.MismatchedArguments(
          Seq(ArgSig.Simple("first", _, _, _, _)),
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
        case Result.Failure.InvalidArguments(List(
          Result.ParamError.Failed(
          ArgSig.Leftover("nums", _, _),
            Seq("aa"),
            "java.lang.NumberFormatException: For input string: \"aa\""
          ),
          Result.ParamError.Failed(
          ArgSig.Leftover("nums", _, _),
            Seq("bb"),
            "java.lang.NumberFormatException: For input string: \"bb\""
          )
        ))=>
      }

      test - assertMatch(
        check.parseInvoke(List("mixedVariadic", "aa", "bb", "3"))
      ){
        case Result.Failure.InvalidArguments(List(
          Result.ParamError.Failed(
            ArgSig.Simple("first", _, _, _, _),
            Seq("aa"),
            "java.lang.NumberFormatException: For input string: \"aa\""
          )
        ))=>
      }
    }
  }
}
