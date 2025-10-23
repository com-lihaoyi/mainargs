package mainargs
import utest._

trait CommandList {
  @main
  def list(@arg v: String): String = v
}

trait CommandCopy {
  @main
  def copy(@arg from: String, @arg to: String): (String, String) = (from, to)
}

object Joined extends CommandCopy with CommandList {
  @main
  def test(@arg from: String, @arg to: String): (String, String) = (from, to)
}

object MultiTraitTests extends TestSuite {
  val check = new Checker(Parser(Joined), allowPositional = true)
  val tests = Tests {
    test - check(List("copy", "fromArg", "toArg"), Result.Success(("fromArg", "toArg")))
    test - check(List("test", "fromArg", "toArg"), Result.Success(("fromArg", "toArg")))
    test - check(List("list", "vArg"), Result.Success("vArg"))
  }
}
