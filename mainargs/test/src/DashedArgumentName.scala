package mainargs
import utest._

object DashedArgumentName extends TestSuite {

  object Base {
    @main
    def `opt-for-18+name`(`opt-for-18+arg`: Boolean) = `opt-for-18+arg`

    @main
    def `opt-for-29+name`(`opt-for-29+arg`: Boolean) = `opt-for-29+arg`

    @main
    def camelOptFor29Name(camelOptFor29Arg: Boolean) = camelOptFor29Arg

    @main(name = "camelOptFor29NameForce")
    def camelOptFor29NameForce(@arg(name =
      "camelOptFor29ArgForce"
    ) camelOptFor29ArgForce: Boolean) = camelOptFor29ArgForce
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = true)
  val snakeCaseCheck = new Checker(
    ParserForMethods(Base),
    allowPositional = true,
    nameMapper = Util.snakeCaseNameMapper
  )

  val tests = Tests {
    test("backticked") {
      test - check(
        List("opt-for-18+name", "--opt-for-18+arg", "true"),
        Result.Success(true)
      )
      test - check(
        List("opt-for-18+name", "--opt-for-18+arg", "false"),
        Result.Success(false)
      )
      test - check(
        List("opt-for-29+name", "--opt-for-29+arg", "true"),
        Result.Success(true)
      )
      test - check(
        List("opt-for-29+name", "--opt-for-29+arg", "false"),
        Result.Success(false)
      )
    }
    test("camelKebabNameMapped") {
      test("mapped") - check(
        List("camel-opt-for-29-name", "--camel-opt-for-29-arg", "false"),
        Result.Success(false)
      )

      // Make sure we continue to support un-mapped names for backwards compatibility
      test("backwardsCompatUnmapped") - check(
        List("camelOptFor29Name", "--camelOptFor29Arg", "false"),
        Result.Success(false)
      )

      test("explicitNameUnmapped") - check(
        List("camelOptFor29NameForce", "--camelOptFor29ArgForce", "false"),
        Result.Success(false)
      )

      // For names given explicitly via `main(name = ...)` or `arg(name = ...)`, we
      // do not use a name mapper, since we assume the user would provide the exact
      // name they want.
      test("explicitMainNameMappedFails") - check(
        List("camel-opt-for-29-name-force", "--camel-opt-for-29-arg-force", "false"),
        Result.Failure.Early.UnableToFindSubcommand(
          List(
            "opt-for-18+name",
            "opt-for-29+name",
            "camel-opt-for-29-name",
            "camelOptFor29NameForce"
          ),
          "camel-opt-for-29-name-force"
        )
      )
      test("explicitArgNameMappedFails") - check(
        List("camelOptFor29NameForce", "--camel-opt-for-29-arg-force", "false"),
        Result.Failure.MismatchedArguments(
          Vector(
            new ArgSig(
              Some("camelOptFor29ArgForce"),
              Some("camelOptFor29ArgForce"),
              None,
              None,
              None,
              mainargs.TokensReader.BooleanRead,
              positional = false,
              hidden = false
            )
          ),
          List("--camel-opt-for-29-arg-force", "false"),
          List(),
          None
        )
      )
    }
    test("camelSnakeNameMapped") {
      test("mapped") - snakeCaseCheck(
        List("camel_opt_for_29_name", "--camel_opt_for_29_arg", "false"),
        Result.Success(false)
      )

      // Make sure we continue to support un-mapped names for backwards compatibility
      test("backwardsCompatUnmapped") - check(
        List("camelOptFor29Name", "--camelOptFor29Arg", "false"),
        Result.Success(false)
      )

      test("explicitNameUnmapped") - check(
        List("camelOptFor29NameForce", "--camelOptFor29ArgForce", "false"),
        Result.Success(false)
      )
    }
  }
}
