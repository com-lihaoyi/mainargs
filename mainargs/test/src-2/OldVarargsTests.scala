package mainargs
import utest._

object OldVarargsTests extends VarargsTests{
  object Base{

    @main
    def pureVariadic(nums: Int*) = nums.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: String*) = first + args.mkString
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
  val isNewVarargsTests = false
}
