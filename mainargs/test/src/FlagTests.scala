package mainargs
import utest._

object FlagTests extends TestSuite {

  object Base {
    @main
    def flaggy(a: Flag, b: Boolean, c: Flag) = Seq(a.value, b, c.value)
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {
    test - check(
      List("-b", "true"),
      Result.Success(Seq(false, true, false))
    )
    test - check(
      List("-b", "false"),
      Result.Success(Seq(false, false, false))
    )

    test - check(
      List("-a", "-b", "false"),
      Result.Success(Seq(true, false, false))
    )

    test - check(
      List("-c", "-b", "false"),
      Result.Success(Seq(false, false, true))
    )

    test - check(
      List("-a", "-c", "-b", "false"),
      Result.Success(Seq(true, false, true))
    )

    test("combined"){
      test - check(
        List("-bfalse"),
        Result.Success(List(false, false, false))
      )
      test - check(
        List("-btrue"),
        Result.Success(List(false, true, false))
      )

      test - check(
        List("-abtrue"),
        Result.Success(List(true, true, false))
      )
      test - check(
        List("-abfalse"),
        Result.Success(List(true, false, false))
      )

      test - check(
        List("-a", "-btrue"),
        Result.Success(List(true, true, false))
      )

      test - check(
        List("-a", "-bfalse"),
        Result.Success(List(true, false, false))
      )

      test - check(
        List("-acbtrue"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("-acbfalse"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("-a", "-c", "-btrue"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("-a", "-c", "-bfalse"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("-a", "-btrue", "-c"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("-a", "-bfalse", "-c"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("-ba"),
        Result.Failure.InvalidArguments(
          List(
            Result.ParamError.Failed(
              new ArgSig(None, Some('b'), None, None, mainargs.TokensReader.BooleanRead, false, false),
              Vector("a"),
              "java.lang.IllegalArgumentException: For input string: \"a\""
            )
          )
        )
      )

      test - check(
        List("-ab"),
        Result.Failure.MismatchedArguments(
          Nil,
          Nil,
          Nil,
          Some(
            new ArgSig(None, Some('b'), None, None, TokensReader.BooleanRead, false, false)
          )
        )
      )
    }

  }
}
