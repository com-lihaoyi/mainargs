package testcustom
import mainargs.{main, arg, ParserForMethods, TokensReader}

object Main{
  implicit object PathRead extends TokensReader[os.Path](
    "path",
    strs => Right(os.Path(strs.head, os.pwd))
  )
  @main
  def run(from: os.Path, to: os.Path) = {
    println("from: " + from)
    println("to:   " + to)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}