package mainargs
import utest._

object OptionSeqTests extends TestSuite {
  object Main {
    @main
    def runOpt(opt: Option[Int]) = opt

    @main
    def runSeq(seq: Seq[Int]) = seq

    @main
    def runVec(seq: Vector[Int]) = seq

    @main
    def runMap(map: Map[String, String]) = map

    @main
    def runInt(int: Int) = int
  }

  val tests = Tests {
    test("opt") {
      test {
        Parser(Main).runOrThrow(Array("runOpt")) ==>
          None
      }
      test {
        Parser(Main).runOrThrow(Array("runOpt", "--opt", "123")) ==>
          Some(123)
      }
    }
    test("seq") {

      test {
        Parser(Main).runOrThrow(Array("runSeq")) ==>
          Seq()
      }
      test {
        Parser(Main).runOrThrow(Array("runSeq", "--seq", "123")) ==>
          Seq(123)
      }
    }
    test("vec") {
      Parser(Main).runOrThrow(Array("runVec", "--seq", "123", "--seq", "456")) ==>
        Vector(123, 456)
    }

    test("map") {

      test {
        Parser(Main).runOrThrow(Array("runMap")) ==>
          Map()
      }
      test {
        Parser(Main).runOrThrow(Array("runMap", "--map", "abc=123")) ==>
          Map("abc" -> "123")
      }
      test {
        Parser(Main).runOrThrow(
          Array("runMap", "--map", "abc=123", "--map", "def=456")
        ) ==>
          Map("abc" -> "123", "def" -> "456")
      }
    }
    test("allowRepeats") {
      test("true") {
        Parser(Main)
          .runOrThrow(Array("runInt", "--int", "123", "--int", "456"), allowRepeats = true) ==>
          456
      }
      test("false") {
        intercept[Exception] {
          Parser(Main)
            .runOrThrow(Array("runInt", "--int", "123", "--int", "456"), allowRepeats = false)
        }
      }
    }
  }
}
