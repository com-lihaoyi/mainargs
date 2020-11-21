package testvararg
import mainargs.{main, arg, ParserForMethods, LeftoverTokens}

object Main{
  @main
  def run(foo: String,
          myNum: Int = 2,
          rest: LeftoverTokens[String]) = {
    println(foo * myNum + " " + rest.value)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}