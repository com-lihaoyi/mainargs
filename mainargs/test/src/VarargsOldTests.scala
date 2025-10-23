package mainargs
import utest._

object VarargsOldTests extends VarargsBaseTests {
  object Base {

    @main
    def pureVariadic(nums: Int*) = nums.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: String*) =
      first.toString + args.mkString
  }

  val check = new Checker(Parser(Base), allowPositional = true)
  val isNewVarargsTests = false
}
