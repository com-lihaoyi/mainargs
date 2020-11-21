package testclass
import mainargs.{main, arg, ParserForClass}

object Main{
  @main
  case class Config(@arg(short = 'f', doc = "String to print repeatedly")
                    foo: String,
                    @arg(name = "my-num", doc = "How many times to print string")
                    myNum: Int = 2,
                    @arg(flag = true, doc = "Example flag")
                    bool: Boolean)
  def main(args: Array[String]): Unit = {
    val config = ParserForClass[Config].constructOrExit(args)
    println(config)
  }
}