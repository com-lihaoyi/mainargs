package mainargs
import utest._


object WrappedVarargsTests extends VarargsTests {
  // Test that we are able to wrap the `Leftover` type we use for Varargs in
  // our own custom types, and have things work
  class Wrapper[T](val unwrap: T)
  class WrapperRead[T](implicit val wrapped: TokensReader[T]) extends TokensReader[Wrapper[T]](
    wrapped.shortName,
    args => wrapped.read(args).map(new Wrapper(_)),
    wrapped.alwaysRepeatable,
    wrapped.allowEmpty,
    wrapped.isLeftover,
  )

  implicit def WrapperRead[T: TokensReader]: TokensReader[Wrapper[T]] = new WrapperRead[T]

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
