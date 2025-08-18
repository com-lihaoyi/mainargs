package mainargs
import utest._

object FlagTests extends TestSuite {

  object Base {
    @main
    def bool(a: Flag, b: Boolean, c: Flag) = Seq(a.value, b, c.value)
    @main
    def str(a: Flag, b: String) = Seq(a.value, b)
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)

  val tests = Tests {
    test - check(
      List("bool", "-b", "true"),
      Result.Success(Seq(false, true, false))
    )
    test - check(
      List("bool", "-b", "false"),
      Result.Success(Seq(false, false, false))
    )

    test - check(
      List("bool", "-a", "-b", "false"),
      Result.Success(Seq(true, false, false))
    )

    test - check(
      List("bool", "-c", "-b", "false"),
      Result.Success(Seq(false, false, true))
    )

    test - check(
      List("bool", "-a", "-c", "-b", "false"),
      Result.Success(Seq(true, false, true))
    )

    test("combined") {
      test - check(
        List("bool", "-bfalse"),
        Result.Success(List(false, false, false))
      )
      test - check(
        List("bool", "-btrue"),
        Result.Success(List(false, true, false))
      )

      test - check(
        List("bool", "-abtrue"),
        Result.Success(List(true, true, false))
      )
      test - check(
        List("bool", "-abfalse"),
        Result.Success(List(true, false, false))
      )

      test - check(
        List("bool", "-a", "-btrue"),
        Result.Success(List(true, true, false))
      )

      test - check(
        List("bool", "-a", "-bfalse"),
        Result.Success(List(true, false, false))
      )

      test - check(
        List("bool", "-acbtrue"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("bool", "-acbfalse"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("bool", "-a", "-c", "-btrue"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("bool", "-a", "-c", "-bfalse"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("bool", "-a", "-btrue", "-c"),
        Result.Success(List(true, true, true))
      )

      test - check(
        List("bool", "-a", "-bfalse", "-c"),
        Result.Success(List(true, false, true))
      )

      test - check(
        List("bool", "-ba"),
        Result.Failure.InvalidArguments(
          List(
            Result.ParamError.Failed(
              new ArgSig(
                None,
                Some('b'),
                None,
                None,
                mainargs.TokensReader.BooleanRead,
                false,
                false
              ),
              Vector("a"),
              "java.lang.IllegalArgumentException: For input string: \"a\""
            )
          )
        )
      )

      test - check(
        List("bool", "-ab"),
        Result.Failure.MismatchedArguments(
          Nil,
          Nil,
          Nil,
          Some(new ArgSig(None, Some('b'), None, None, TokensReader.BooleanRead, false, false))
        )
      )

      test - check(List("str", "-b=value", "-a"), Result.Success(List(true, "value")))
      test - check(List("str", "-b=", "-a"), Result.Success(List(true, "")))

      test - check(List("str", "-ab=value"), Result.Success(List(true, "value")))

      test - check(
        List("str", "-bvalue", "-akey=value"),
        Result.Failure.MismatchedArguments(Nil, List("-akey=value"), Nil, None)
      )
    }

  }
}
