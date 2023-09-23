package mainargs
import utest._


object RenderedTests extends TestSuite{
  @main
  case class Config(
    a: Int,
    b: Int = 0,
    c: Option[Int] = None,
    d: Seq[Int],
    e: Seq[Int] = Seq.empty,
    @arg(doc = "this one has a doc")
    a1: Int,
    @arg(doc = "this one has a doc")
    b1: Int = 0,
    @arg(doc = "this one has a doc")
    c1: Option[Int] = None,
    @arg(doc = "this one has a doc")
    d1: Seq[Int],
    @arg(doc = "this one has a doc")
    e1: Seq[Int] = Seq.empty
  )
  val parser = ParserForClass[Config]

  val tests = Tests {
    test("formatMainMethods"){
      val parsed = parser.helpText()
      val expected =
        """apply
          |   -a <int>
          |   --a1 <int>    this one has a doc
          |  [-b <int>]
          |  [--b1 <int>]   this one has a doc
          |  [-c <int>]
          |  [--c1 <int>]   this one has a doc
          |  [-d <int>]*
          |  [--d1 <int>]*  this one has a doc
          |  [-e <int>]*
          |  [--e1 <int>]*  this one has a doc
          |""".stripMargin

      parsed ==> expected
    }
  }
}
