package mainargs
import utest._

object VarargsCustomTests extends VarargsBaseTests {
  // Test that we are able to replace the `Leftover` type entirely with our
  // own implementation
  class Wrapper[T](val unwrap: Seq[T])
  class WrapperRead[T](implicit val wrapped: TokensReader.Simple[T])
      extends TokensReader.Leftover[Wrapper[T], T] {
    def read(strs: Seq[String]) = {
      val results = strs.map(s => implicitly[TokensReader.Simple[T]].read(Seq(s)))
      val failures = results.collect { case Left(x) => x }
      val successes = results.collect { case Right(x) => x }

      if (failures.nonEmpty) Left(failures.head)
      else Right(new Wrapper(successes))
    }
  }

  implicit def WrapperRead[T: TokensReader.Simple]: TokensReader[Wrapper[T]] =
    new WrapperRead[T]

  object Base {
    @main
    def pureVariadic(nums: Wrapper[Int]) = nums.unwrap.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: Wrapper[String]) = {
      first + args.unwrap.mkString
    }
    @main
    def mixedVariadicWithDefault(
        @arg(short = 'f') first: Int = 1337,
        args: Wrapper[String]
    ) = {
      first + args.unwrap.mkString
    }
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
  val isNewVarargsTests = true
}
