package example.hello
import mainargs.{main, arg, ParserForMethods, Flag}

object Main{
  @main
  def run(@arg(short = 'f', doc = "String to print repeatedly")
          foo: String,
          @arg(name = "my-num", doc = "How many times to print string")
          myNum: Int = 2,
          @arg(doc = "Example flag")
          bool: Flag) = {
    println(foo * myNum + " " + bool.value)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}