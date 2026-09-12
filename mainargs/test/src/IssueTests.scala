package mainargs
import utest._

object IssueTests extends TestSuite {

  object MultiMain {
    @main
    def foo() = 1

    @main
    def bar(i: Int) = i
  }

  object Main {
    @main
    def mycmd(@arg(name = "the-flag") f: mainargs.Flag = mainargs.Flag(false),
              @arg str: String = "s",
              args: Leftover[String]) = {
      (f.value, str, args.value)
    }
  }

  val tests = Tests {
    test("issue190") {
      val parser = Parser(MultiMain)
      val help = parser.helpText()
      test("missingSubcommand") {
        parser.runEither(Seq()) match {
          case Left(msg) =>
            assert(msg.startsWith("Need to specify a sub command: foo, bar"))
            assert(msg.endsWith(help))
          case _ => assert(false)
        }
      }
      test("subcommandParseFailure") {
        parser.runEither(Seq("bar")) match {
          case Left(msg) =>
            assert(msg.contains("Missing argument: -i"))
            assert(msg.endsWith(help))
          case _ => assert(false)
        }
      }
      test("printHelpOnExitDisabled") {
        parser.runEither(Seq(), printHelpOnExit = false) ==>
          Left("Need to specify a sub command: foo, bar")
      }
    }
    test("issue60") {
      test {
        val parsed = Parser(Main)
          .runEither(Seq("--str", "str", "a", "b", "c", "d"), allowPositional = true)

        assert(parsed == Right((false, "str", List("a", "b", "c", "d"))))
      }
      test {
        val parsed = Parser(Main)
          .runEither(Seq("a", "b", "c", "d"), allowPositional = true)

        assert(parsed == Right((false, "a", List("b", "c", "d"))))
      }
    }
  }
}
