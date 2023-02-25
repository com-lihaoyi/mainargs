package example.vararg
import mainargs.{main, arg, ParserForMethods, Leftover}

object Main {
  @main
  def run(foo: String, myNum: Int, rest: String*) = {
    println(foo * myNum + " " + rest.value)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
