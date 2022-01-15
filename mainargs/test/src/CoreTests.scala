package mainargs

import mainargs.Result.Failure.MismatchedArguments
import utest._



object CoreBase{
  case object MyException extends Exception
  @main
  def foo() = 1
  @main
  def bar(i: Int) = i

  @main(doc = "Qux is a function that does stuff")
  def qux(i: Int,
          @arg(doc = "Pass in a custom `s` to override it")
          s: String  = "lols") = s * i
  @main
  def ex() = throw MyException

  def notExported(nonParseable: java.io.InputStream) = ???

  val alsoNotExported = "bazzz"
}

object CorePositionalEnabledTests extends CoreTests(true)
object CorePositionalDisabledTests extends CoreTests(false)

class CoreTests(allowPositional: Boolean) extends TestSuite{
  val check = new Checker(ParserForMethods(CoreBase), allowPositional = allowPositional)

  val tests = Tests {
    test("formatMainMethods"){
      val parsed = check.parser.helpText()
      val expected =
        """Available subcommands:
          |
          |  foo
          |
          |  bar
          |     -i <int>
          |
          |  qux
          |  Qux is a function that does stuff
          |     -i <int>
          |    [-s <str>]  Pass in a custom `s` to override it
          |
          |  ex
          |""".stripMargin

      parsed ==> expected
    }
    test("basicModelling") {
      val names = check.mains.value.map(_.name)

      assert(
        names ==
        List("foo", "bar", "qux", "ex")
      )
      val evaledArgs = check.mains.value.map(_.argSigs.map{
        case ArgSig.Simple(name, s, docs, None, parser, _) => (s, docs, None, parser)
        case ArgSig.Simple(name, s, docs, Some(default), parser, _) =>
          (s, docs, Some(default(CoreBase)), parser)
      })

      assert(
        evaledArgs == List(
          List(),
          List((Some('i'), None, None, TokensReader.IntRead)),
          List(
            (Some('i'), None, None, TokensReader.IntRead),
            (Some('s'), Some("Pass in a custom `s` to override it"), Some("lols"), TokensReader.StringRead)
          ),
          List()
        )
      )
    }

    test("invoke"){
      test - check(
        List("foo"), Result.Success(1)
      )
      test - check(
        List("bar", "-i", "2"), Result.Success(2)
      )
      test - check(
        List("qux", "-i", "2"), Result.Success("lolslols")
      )
      test - check(
        List("qux", "-i", "3", "-s", "x"), Result.Success("xxx")
      )
      test - check(
        List("qux", "-i", "3", "-s", "-"), Result.Success("---")
      )
      test - check(
        List("qux", "-i", "3", "-s", "--"), Result.Success("------")
      )
    }

    test("failures"){
      test("missingParams"){
        test - assertMatch(check.parseInvoke(List("bar"))){
          case Result.Failure.MismatchedArguments(
            Seq(ArgSig.Simple(None, Some('i'), _, _, _, _)),
            Nil,
            Nil,
            None
          ) =>
        }
        test - assertMatch(check.parseInvoke(List("qux", "-s", "omg"))){
          case Result.Failure.MismatchedArguments(
            Seq(ArgSig.Simple(None, Some('i'), _, _, _, _)),
            Nil,
            Nil,
            None
          ) =>
        }
      }

      test("tooManyParams") - check(
        List("foo", "1", "2"),
        Result.Failure.MismatchedArguments(Nil, List("1", "2"), Nil, None)
      )

      test("failing") - check(
        List("ex"),
        Result.Failure.Exception(CoreBase.MyException)
      )
    }
  }
}


object CorePositionalDisabledOnlyTests extends TestSuite{
  val check = new Checker(ParserForMethods(CoreBase), allowPositional = false)

  val tests = Tests {
    test("invoke"){
      test - check(
        List("bar", "2"),
        MismatchedArguments(
          missing = List(ArgSig.Simple(None,Some('i'),None,None, TokensReader.IntRead, false)),
          unknown = List("2")
        )
      )
      test - check(
        List("qux", "2"),
        MismatchedArguments(
          missing = List(ArgSig.Simple(None,Some('i'),None,None, TokensReader.IntRead, false)),
          unknown = List("2")
        )
      )
      test - check(
        List("qux", "3", "x"),
        MismatchedArguments(
          missing = List(ArgSig.Simple(None,Some('i'),None,None, TokensReader.IntRead, false)),
          unknown = List("3", "x")
        )
      )
      test - check(
        List("qux", "-i", "3", "x"),
        MismatchedArguments(List(),List("x"),List(),None)
      )
    }

    test("failures"){
      test("invalidParams") - check(
        List("bar", "lol"),
        MismatchedArguments(
          missing = List(ArgSig.Simple(None,Some('i'),None,None, TokensReader.IntRead, false)),
          unknown = List("lol"),
        )
      )
    }

    test("redundantParams") - check(
      List("qux", "1", "-i", "2"),
      MismatchedArguments(
        missing = List(ArgSig.Simple(None, Some('i'),None,None, TokensReader.IntRead, false)),
        unknown = List("1", "-i", "2"),
      )
    )
  }
}

object CorePositionalEnabledOnlyTests extends TestSuite{
  val check = new Checker(ParserForMethods(CoreBase), allowPositional = true)

  val tests = Tests {
    test("invoke"){
      test - check(List("bar", "2"), Result.Success(2))
      test - check(List("qux", "2"), Result.Success("lolslols"))
      test - check(List("qux", "3", "x"), Result.Success("xxx"))
      test - check(List("qux", "2", "-"), Result.Success("--"))
      test - check(List("qux", "2", "--"), Result.Success("----"))
      test - check(List("qux", "1", "---"), Result.Success("---"))
      test - check(
        List("qux", "-i", "3", "x"), Result.Success("xxx")
      )
    }

    test("failures"){
      test("invalidParams") - assertMatch(
        check.parseInvoke(List("bar", "lol"))
      ){
        case Result.Failure.InvalidArguments(
        List(Result.ParamError.Failed(ArgSig.Simple(None, Some('i'), _, _, _, _), Seq("lol"), _))
        ) =>
      }

      test("redundantParams"){
        val parsed = check.parseInvoke(List("qux", "1", "-i", "2"))
        assertMatch(parsed){
          case Result.Failure.MismatchedArguments(
          Nil, Nil, Seq((ArgSig.Simple(None, Some('i'), _, _, _, _), Seq("1", "2"))), None
          ) =>
        }
      }
    }
  }
}
