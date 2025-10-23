package mainargs
import utest._
object VarargsNewTests extends VarargsBaseTests {
  object Base {
    @main
    def pureVariadic(nums: Leftover[Int]) = nums.value.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: Leftover[String]) = {
      first + args.value.mkString
    }
    @main
    def mixedVariadicWithDefault(
        @arg(short = 'f') first: Int = 1337,
        args: Leftover[String]
    ) = {
      first + args.value.mkString
    }
  }

  val check = new Checker(Parser(Base), allowPositional = true)
  val isNewVarargsTests = true
}
