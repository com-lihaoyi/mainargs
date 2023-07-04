package example.hello2
import mainargs.{main, arg, ParserForMethods, Flag}

object Main {
  @main
  def foo(
      @arg(short = 'f', doc = "String to print repeatedly")
      foo: String,
      @arg(name = "my-num", doc = "How many times to print string")
      myNum: Int = 2,
      @arg(doc = "Example flag")
      bool: Flag
  ) = {
    println(foo * myNum + " " + bool.value)
  }
  @main
  def bar(
      i: Int,
      @arg(doc = "Pass in a custom `s` to override it")
      s: String = "lols"
  ) = {
    println(s * i)
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
