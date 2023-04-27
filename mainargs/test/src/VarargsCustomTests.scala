package mainargs
import utest._


object VarargsCustomTests extends VarargsBaseTests {
  // Test that we are able to replace the `Leftover` type entirely with our
  // own implementation
  class Wrapper[T](val unwrap: Seq[T])
  class WrapperRead[T](implicit val wrapped: TokensReader[T]) extends TokensReader.Leftover[Wrapper[T], T]{
    def read(strs: Seq[String]) = {
      val (failures, successes) = strs
        .map(s => implicitly[TokensReader[T]].read(Seq(s)))
        .partitionMap(identity)

      if (failures.nonEmpty) Left(failures.head)
      else Right(new Wrapper(successes))
    }
  }

  implicit def WrapperRead[T: TokensReader]: TokensReader[Wrapper[T]] = new WrapperRead[T]

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
