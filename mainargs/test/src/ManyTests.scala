package mainargs
import utest._


object ManyTests extends TestSuite{
  @main
  case class Config(a: String, b: Int, c: Boolean,
                    d: String, e: Int, f: Boolean,
                    g: String, h: Int, i: Boolean,
                    j: String, k: Int, l: Boolean)

  val parser = ParserForClass[Config]
  val tests = Tests {
    test {
      parser.constructEither(
        Array(
          "--a", "A", "--b", "1", "--c", "true",
          "--d", "D", "--e", "2", "--f", "false",
          "--g", "G", "--h", "3", "--i", "true",
          "--j", "J", "--k", "4", "--l", "false",
        ),
        allowPositional = true
      )
    }
    test {
      parser.constructEither(
        Array(
          "A", "--b", "1", "--c", "true",
          "D", "--e", "2", "--f", "false",
          "G", "--h", "3", "--i", "true",
          "J", "--k", "4", "--l", "false",
        ),
        allowPositional = true
      )
    }
    test {
      parser.constructEither(
        Array(
          "A", "1", "--c", "true",
          "D", "2", "--f", "false",
          "G", "3", "--i", "true",
          "J", "4", "--l", "false",
        ),
        allowPositional = true
      )
    }
    test {
      parser.constructEither(
        Array(
          "A", "1", "true",
          "D", "2", "false",
          "G", "3", "true",
          "J", "4", "false",
        ),
        allowPositional = true
      )
    }
  }
}
