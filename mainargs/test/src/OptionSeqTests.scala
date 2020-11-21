package mainargs
import utest._


object OptionSeqTests extends TestSuite{
  object Main{
    @main
    def runOpt(opt: Option[Int]) = opt

    @main
    def runSeq(seq: Seq[Int]) = seq
  }

  val tests = Tests {
    test("opt") {
      test {
        ParserForMethods(Main).runOrThrow(Array("runOpt")) ==>
          None
      }
      test {
        ParserForMethods(Main).runOrThrow(Array("runOpt", "--opt", "123")) ==>
          Some(123)
      }
    }
    test("seq") {

      test {
        ParserForMethods(Main).runOrThrow(Array("runSeq")) ==>
          Seq()
      }
      test {
        ParserForMethods(Main).runOrThrow(Array("runSeq", "--seq", "123")) ==>
          Seq(123)
      }
      test {
        ParserForMethods(Main).runOrThrow(Array("runSeq", "--seq", "123", "--seq", "456")) ==>
          Seq(123, 456)
      }
    }
  }
}
