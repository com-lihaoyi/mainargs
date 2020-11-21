package testhello
import mainargs.{main, arg, ParserForMethods}

object Main{
  @main
  def run(@arg(short = 'f', doc = "String to print repeatedly")
          foo: String,
          @arg(name = "my-num", doc = "How many times to print string")
          myNum: Int = 2,
          @arg(flag = true, doc = "Example flag")
          bool: Boolean) = {
    println(foo * myNum + " " + bool)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}