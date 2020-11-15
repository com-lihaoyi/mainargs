package mainargs

import utest._

object PositionalAgnosticEnabledTests extends PositionalAgnosticTests(true)
object PositionalAgnosticDisabledTests extends PositionalAgnosticTests(false)

class PositionalAgnosticTests(allowPositional: Boolean) extends TestSuite{
  val check = new Checker[MultiTarget.type](allowPositional = allowPositional)

  val tests = Tests {
    test("router"){

      test("formatMainMethods"){
        Renderer.formatMainMethods(MultiTarget, check.mains.value, 95)
      }
      test("basicModelling") {
        val names = check.mains.value.map(_.name)
        assert(
          names ==
          List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic", "flaggy")
        )
        val evaledArgs = check.mains.value.map(_.argSigs.map{
          case ArgSig(name, s, tpe, docs, None, _, _) => (name, tpe, docs, None)
          case ArgSig(name, s, tpe, docs, Some(default), _, _) =>
            (name, tpe, docs, Some(default(MultiTarget)))
        })

        assert(
          evaledArgs == List(
            List(),
            List(("i", "int", None, None)),
            List(
              ("i", "int", None, None),
              ("s", "str", Some("Pass in a custom `s` to override it"), Some("lols"))
            ),
            List(),
            List(("nums", "int", None, None)),
            List(("first", "int", None, None), ("args", "str", None, None)),
            List(
              ("a", "bool", None, Some(false)),
              ("b", "bool", None, None),
              ("c", "bool", None, Some(false))
            )
          )
        )
      }

      test("invoke"){
        test - check(
          List("foo"), Result.Success(1)
        )
        test - check(
          List("bar", "--i", "2"), Result.Success(2)
        )
        test - check(
          List("qux", "--i", "2"), Result.Success("lolslols")
        )
        test - check(
          List("qux", "--i", "3", "--s", "x"), Result.Success("xxx")
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            List("pureVariadic", "1", "2", "3"), Result.Success(6)
          )
        }
        test("emptyVarargsPasses"){
          test - check(List("pureVariadic"), Result.Success(0))
          test - check(
            List("mixedVariadic", "-f", "1"), Result.Success("1")
          )
        }
        test("varargsAreAlwaysPositional"){
          val invoked = check.parseInvoke(
            List("pureVariadic", "--nums", "31337")
          )
          test - assertMatch(invoked){
            case Result.Error.InvalidArguments(List(
              Result.ParamError.Failed(
              ArgSig("nums", _, "int", _, _, true, _),
              "--nums",
              """java.lang.NumberFormatException: For input string: "--nums""""
            )
            ))=>
          }

          test - assertMatch(
            check.parseInvoke(List("pureVariadic", "1", "2", "3", "--nums", "4"))
          ){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Failed(
            ArgSig("nums", _, "int", _, _, true, _),
            "--nums",
            "java.lang.NumberFormatException: For input string: \"--nums\""
            )
            ))=>
          }
        }

        test("notEnoughNormalArgsStillFails"){
          assertMatch(check.parseInvoke(List("mixedVariadic"))){
            case Result.Error.MismatchedArguments(
              List(ArgSig("first", _, _, _, _, false, _)),
              Nil,
              Nil,
              None
            ) =>
          }
        }
        test("multipleVarargParseFailures"){
          test - assertMatch(
            check.parseInvoke(List("pureVariadic", "aa", "bb", "3"))
          ){
            case Result.Error.InvalidArguments(List(
              Result.ParamError.Failed(
                ArgSig("nums", _, "int", _, _, true, _),
                "aa",
                "java.lang.NumberFormatException: For input string: \"aa\""
              ),
              Result.ParamError.Failed(
                ArgSig("nums", _, "int", _, _, true, _),
                "bb",
                "java.lang.NumberFormatException: For input string: \"bb\""
              )
            ))=>
          }
        }
      }


      test("flags"){
        test - check(
          List("flaggy", "--b", "true"), Result.Success(true)
        )
        test - check(
          List("flaggy", "--b", "false"), Result.Success(false)
        )

        test - check(
          List("flaggy", "--a", "--b", "false"), Result.Success(true)
        )

        test - check(
          List("flaggy", "--c", "--b", "false"), Result.Success(true)
        )

        test - check(
          List("flaggy", "--a", "--c", "--b", "false"), Result.Success(true)
        )
      }

      test("failures"){
        test("missingParams"){
          test - assertMatch(check.parseInvoke(List("bar"))){
            case Result.Error.MismatchedArguments(
              List(ArgSig("i", _, _, _, _, false, _)),
              Nil,
              Nil,
              None
            ) =>
          }
          test - assertMatch(check.parseInvoke(List("qux", "--s", "omg"))){
            case Result.Error.MismatchedArguments(
            List(ArgSig("i", _, _, _, _, false, _)),
              Nil,
              Nil,
              None
            ) =>
          }
        }

        test("tooManyParams") - check(
          List("foo", "1", "2"),
          Result.Error.MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )

        test("failing") - check(
          List("ex"),
          Result.Error.Exception(MyException)
        )
      }
    }
  }
}
