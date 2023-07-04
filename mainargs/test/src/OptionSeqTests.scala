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

    @main
    def runOptionSeq(os: Option[Seq[Int]]) = os
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
    }

    test("option seq") {
      test {
        ParserForMethods(Main).runOrThrow(Array("runOptionSeq", "--os", "123")) ==>
          Some(Seq(123))
      }

      test {
        ParserForMethods(Main).runOrThrow(Array("runOptionSeq", "--os", "123", "--os", "456")) ==>
          Some(Seq(123, 456))
      }

      test {
        ParserForMethods(Main).runOrThrow(Array("runOptionSeq")) ==>
          None
      }
    }

    test("vec") {
      ParserForMethods(Main).runOrThrow(Array("runVec", "--seq", "123", "--seq", "456")) ==>
        Vector(123, 456)
    }

    test("map") {

      test {
        ParserForMethods(Main).runOrThrow(Array("runMap")) ==>
          Map()
      }
      test {
        ParserForMethods(Main).runOrThrow(Array("runMap", "--map", "abc=123")) ==>
          Map("abc" -> "123")
      }
      test {
        ParserForMethods(Main).runOrThrow(
          Array("runMap", "--map", "abc=123", "--map", "def=456")
        ) ==>
          Map("abc" -> "123", "def" -> "456")
      }
    }
    test("allowRepeats") {
      test("true") {
        ParserForMethods(Main)
          .runOrThrow(Array("runInt", "--int", "123", "--int", "456"), allowRepeats = true) ==>
          456
      }
      test("false") {
        intercept[Exception] {
          ParserForMethods(Main)
            .runOrThrow(Array("runInt", "--int", "123", "--int", "456"), allowRepeats = false)
        }
      }
    }
  }
}
