package testclassarg
import mainargs.{main, arg, ParserForMethods, ParserForClass}

object Main{
  @main
  case class Config(@arg(short = 'f', doc = "String to print repeatedly")
                    foo: String,
                    @arg(name = "my-num", doc = "How many times to print string")
                    myNum: Int = 2,
                    @arg(flag = true, doc = "Example flag")
                    bool: Boolean)
  implicit def configParser = ParserForClass[Config]

  @main
  def bar(config: Config,
          @arg(name = "extra-message")
          extraMessage: String) = {
    println(config.foo * config.myNum + " " + config.bool + " " + extraMessage)
  }
  @main
  def qux(config: Config,
          n: Int) = {
    println((config.foo * config.myNum + " " + config.bool + "\n") * n)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}