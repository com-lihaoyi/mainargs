package mainargs

case object MyException extends Exception
object Target{
  @main
  def foo() = 1
  @main
  def bar(i: Int) = i

  @main(doc = "Qux is a function that does stuff")
  def qux(i: Int,
          @arg(doc = "Pass in a custom `s` to override it")
          s: String  = "lols") = s * i
  @main
  def ex() = throw MyException

  def notExported() = ???

  val baz = "bazzz"

  @main
  def pureVariadic(nums: Int*) = nums.sum

  @main
  def mixedVariadic(@arg(short = 'f') first: Int, args: String*) = first + args.mkString

  @main
  def flaggy(@arg(flag = true) a: Boolean = false,
             b: Boolean,
             @arg(flag = true) c: Boolean = false) = a || b || c
}

