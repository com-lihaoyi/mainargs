package example.hello
import mainargs.{main, arg, ParserForMethods, Flag}

object Main {
  @main
  def bools(a: Flag, b: Boolean = false, c: Flag) = println(Seq(a.value, b, c.value))

  @main
  def strs(a: Flag, b: String) = println(Seq(a.value, b))

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
