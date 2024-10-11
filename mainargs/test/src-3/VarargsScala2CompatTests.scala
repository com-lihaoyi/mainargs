package mainargs
import utest._

object VarargsScala2CompatTests extends VarargsBaseTests {
  object Base {

    // (foo: scala.`<repeated>`[T]) === (foo: T*) in Scala 2 pickles (which can be read from Scala 3)
    // in Scala 3, the equivalent is (foo: Seq[T] @repeated)
    @main
    def pureVariadic(nums: scala.`<repeated>`[Int]) = nums.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: scala.`<repeated>`[String]) =
      first.toString + args.mkString
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
  val isNewVarargsTests = false
}
