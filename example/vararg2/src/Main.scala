package example.vararg2
import mainargs.{main, arg, ParserForClass, Leftover}

object Main {
  @main
  case class Config(foo: String, myNum: Int = 2, rest: Leftover[String])

  def main(args: Array[String]): Unit = {
    val config = ParserForClass[Config].constructOrExit(args)
    println(config)
  }
}
