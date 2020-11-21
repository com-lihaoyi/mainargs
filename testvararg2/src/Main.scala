package testvararg2
import mainargs.{main, arg, ParserForClass, LeftoverTokens}

object Main{
  @main
  case class Config(foo: String,
                    myNum: Int = 2,
                    rest: LeftoverTokens[String])

  def main(args: Array[String]): Unit = {
    val config = ParserForClass[Config].constructOrExit(args)
    println(config)
  }
}