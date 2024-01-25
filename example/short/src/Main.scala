package example.hello
import mainargs.{main, arg, ParserForMethods, Flag}

object Main {
  @main
  def flaggy(a: Flag, b: Boolean = false, c: Flag) = println(Seq(a.value, b, c.value))

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
