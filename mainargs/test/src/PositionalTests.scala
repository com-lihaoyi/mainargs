package mainargs
import utest._


object PositionalTests extends TestSuite{

  object Base{
    @main
    def positional(x: Boolean, @arg(positional = true) y: Boolean, z: Boolean) = (x, y, z)
  }
  val check = new Checker(ParserForMethods(Base), allowPositional = false)

  val tests = Tests {
    test - check(
      List("true", "true", "true"),
      Result.Failure.MismatchedArguments(
        Vector(
          ArgSig.Simple(None,Some('x'),None,None,TokensReader.BooleanRead,false),
          ArgSig.Simple(None,Some('z'),None,None,TokensReader.BooleanRead,false)
        ),
        List("true", "true"),
        List(),
        None
      )
    )
    test - check(
      List("-x", "true", "false", "-z", "false"), Result.Success((true, false, false))
    )
    test - check(
      List("-x", "true", "-y", "false", "-z", "false"),
      Result.Failure.MismatchedArguments(
        Vector(
          ArgSig.Simple(None,Some('y'),None,None,TokensReader.BooleanRead,true),
          ArgSig.Simple(None,Some('z'),None,None,TokensReader.BooleanRead,false)
        ),
        List("-y", "false", "-z", "false"),
        List(),
        None
      )
    )
  }
}
