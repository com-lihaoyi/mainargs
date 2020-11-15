package mainargs

import utest._

object PositionalAgnosticEnabledTests extends PositionalAgnosticTests(true)
object PositionalAgnosticDisabledTests extends PositionalAgnosticTests(false)

class PositionalAgnosticTests(allowPositional: Boolean) extends TestSuite{
  val check = new Checker(allowPositional = allowPositional)

  val tests = Tests {
    test("router"){
      val routes = generateRoutes[MultiTarget.type]

      test("formatMainMethods"){
        Renderer.formatMainMethods(MultiTarget, routes.value, 95)
      }
      test("basicModelling") {
        val names = routes.value.map(_.name)
        assert(
          names ==
          List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic", "flaggy")
        )
        val evaledArgs = routes.value.map(_.argSigs.map{
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
          MultiTarget, routes, List("foo"), Result.Success(1)
        )
        test - check(
          MultiTarget, routes, List("bar", "--i", "2"), Result.Success(2)
        )
        test - check(
          MultiTarget, routes, List("qux", "--i", "2"), Result.Success("lolslols")
        )
        test - check(
          MultiTarget, routes, List("qux", "--i", "3", "--s", "x"), Result.Success("xxx")
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            MultiTarget, routes, List("pureVariadic", "1", "2", "3"), Result.Success(6)
          )
        }
        test("emptyVarargsPasses"){
          test - check(MultiTarget, routes, List("pureVariadic"), Result.Success(0))
          test - check(
            MultiTarget, routes, List("mixedVariadic", "-f", "1"), Result.Success("1")
          )
        }
        test("varargsAreAlwaysPositional"){
          val invoked = check.parseInvoke(
            MultiTarget, routes, List("pureVariadic", "--nums", "31337")
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
            check.parseInvoke(
              MultiTarget,
              routes,
              List("pureVariadic", "1", "2", "3", "--nums", "4")
            )
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
          assertMatch(check.parseInvoke(MultiTarget, routes, List("mixedVariadic"))){
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
            check.parseInvoke(MultiTarget, routes, List("pureVariadic", "aa", "bb", "3"))
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
          MultiTarget, routes, List("flaggy", "--b", "true"), Result.Success(true)
        )
        test - check(
          MultiTarget, routes, List("flaggy", "--b", "false"), Result.Success(false)
        )

        test - check(
          MultiTarget, routes, List("flaggy", "--a", "--b", "false"), Result.Success(true)
        )

        test - check(
          MultiTarget, routes, List("flaggy", "--c", "--b", "false"), Result.Success(true)
        )

        test - check(
          MultiTarget,
          routes,
          List("flaggy", "--a", "--c", "--b", "false"), Result.Success(true)
        )

      }

      test("failures"){
        test("missingParams"){
          test - assertMatch(check.parseInvoke(MultiTarget, routes, List("bar"))){
            case Result.Error.MismatchedArguments(
              List(ArgSig("i", _, _, _, _, false, _)),
              Nil,
              Nil,
              None
            ) =>
          }
          test - assertMatch(check.parseInvoke(MultiTarget, routes, List("qux", "--s", "omg"))){
            case Result.Error.MismatchedArguments(
            List(ArgSig("i", _, _, _, _, false, _)),
              Nil,
              Nil,
              None
            ) =>
          }
        }

        test("tooManyParams") - check(
          MultiTarget, routes, List("foo", "1", "2"),
          Result.Error.MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )

        test("failing") - check(
          MultiTarget,
          routes,
          List("ex"),
          Result.Error.Exception(MyException)
        )
      }
    }
  }
}
