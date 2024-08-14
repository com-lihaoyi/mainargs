package example.optseq
import mainargs.{main, arg, ParserForMethods, TokensReader}

object Main {
  @main
  def runOpt(opt: Option[Int]) = println(opt)

  @main
  def runSeq(seq: Seq[Int]) = println(seq)

  @main
  def runVec(seq: Vector[Int]) = println(seq)

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
