package mainargs
import utest._

object VarargsWrappedTests extends VarargsBaseTests {
  // Test that we are able to wrap the `Leftover` type we use for Varargs in
  // our own custom types, and have things work
  class Wrapper[T](val unwrap: T)
  class WrapperRead[T](implicit wrapped: TokensReader.ShortNamed[T])
      extends TokensReader.Leftover[Wrapper[T], T] {

    def read(strs: Seq[String]) = wrapped
      .asInstanceOf[TokensReader.Leftover[T, _]]
      .read(strs).map(new Wrapper(_))

    def shortName = wrapped.shortName
  }

  implicit def WrapperRead[T: TokensReader.ShortNamed]: TokensReader[Wrapper[T]] =
    new WrapperRead[T]

  object Base {
    @main
    def pureVariadic(nums: Wrapper[Leftover[Int]]) = nums.unwrap.value.sum

    @main
    def mixedVariadic(@arg(short = 'f') first: Int, args: Wrapper[Leftover[String]]) = {
      first + args.unwrap.value.mkString
    }
    @main
    def mixedVariadicWithDefault(
        @arg(short = 'f') first: Int = 1337,
        args: Wrapper[Leftover[String]]
    ) = {
      first + args.unwrap.value.mkString
    }
  }

  val check = new Checker(ParserForMethods(Base), allowPositional = true)
  val isNewVarargsTests = true
}
