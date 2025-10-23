package mainargs
import utest._

object PositionalTests extends TestSuite {

  object Base {
    @main
    def positional(x: Boolean, @arg(positional = true) y: Boolean, z: Boolean) = (x, y, z)
  }
  val check = new Checker(Parser(Base), allowPositional = false)

  val tests = Tests {
    test - check(
      List("true", "true", "true"),
      Result.Failure.MismatchedArguments(
        Vector(
          ArgSig(
            None,
            Some('x'),
            None,
            None,
            TokensReader.BooleanRead,
            positional = false,
            hidden = false
          ),
          ArgSig(
            None,
            Some('z'),
            None,
            None,
            TokensReader.BooleanRead,
            positional = false,
            hidden = false
          )
        ),
        List("true", "true"),
        List(),
        None
      )
    )
    test - check(
      List("-x", "true", "false", "-z", "false"),
      Result.Success((true, false, false))
    )
    test - check(
      List("-x", "true", "-y", "false", "-z", "false"),
      Result.Failure.MismatchedArguments(
        List(
          ArgSig(
            None,
            Some('y'),
            None,
            None,
            TokensReader.BooleanRead,
            positional = true,
            hidden = false
          ),
          ArgSig(
            None,
            Some('z'),
            None,
            None,
            TokensReader.BooleanRead,
            positional = false,
            hidden = false
          )
        ),
        List("-y", "false", "-z", "false"),
        List(),
        None
      )
    )
  }
}
