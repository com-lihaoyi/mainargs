
package mainargs

import utest._

object RouterTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    entryPoint.invoke(base, Util.groupArgs(input))
  }
  def check[B, T](base: B,
                  entryPoint: EntryPoint[B],
                  input: List[String],
                  expected: Result[T]) = {
    val result = parseInvoke(base, entryPoint, input)
    assert(result == expected)
  }

  val tests = Tests {
    test("router"){
      case object MyException extends Exception
      object Target{
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

        def notExported() = ???

        val baz = "bazzz"

        @main
        def pureVariadic(nums: Int*) = nums.sum

        @main
        def mixedVariadic(@arg(short = 'f') first: Int, args: String*) = first + args.mkString
      }
      val routes = generateRoutes[Target.type]

      test("formatMainMethods"){
        Renderer.formatMainMethods(Target, routes)
      }
      test("basicModelling") {
        val names = routes.map(_.name)
        assert(
          names == List("foo", "bar", "qux", "ex", "pureVariadic", "mixedVariadic")
        )
        val evaledArgs = routes.map(_.argSignatures.map{
          case ArgSig(name, s, tpe, docs, None, _) => (name, tpe, docs, None)
          case ArgSig(name, s, tpe, docs, Some(default), _) =>
            (name, tpe, docs, Some(default(Target)))
        })
        assert(
          evaledArgs == List(
            List(),
            List(("i", "Int", None, None)),
            List(
              ("i", "Int", None, None),
              ("s", "String", Some("Pass in a custom `s` to override it"), Some("lols"))
            ),
            List(),
            List(("nums", "Int*", None, None)),
            List(("first", "Int", None, None), ("args", "String*", None, None))
          )
        )
      }


      test("invoke"){
        check(Target, routes(0), List(), Result.Success(1))
        check(Target, routes(1), List("2"), Result.Success(2))
        check(Target, routes(1), List("--i", "2"), Result.Success(2))
        check(Target, routes(2), List("2"), Result.Success("lolslols"))
        check(Target, routes(2), List("--i", "2"), Result.Success("lolslols"))
        check(Target, routes(2), List("3", "x"), Result.Success("xxx"))
        check(Target, routes(2), List("--i", "3", "x"), Result.Success("xxx"))
        check(Target, routes(2), List("--i", "3", "--s", "x"), Result.Success("xxx"))
      }
      test("varargs"){
        test("happyPathPasses"){
          check(Target, routes(4), List("1", "2", "3"), Result.Success(6))
          check(Target, routes(5), List("1", "2", "3", "4", "5"), Result.Success("12345"))
        }
        test("emptyVarargsPasses"){
          check(Target, routes(4), List(), Result.Success(0))
          check(Target, routes(5), List("1"), Result.Success("1"))
        }
        test("varargsAreAlwaysPositional"){
          val invoked = parseInvoke(Target, routes(4), List("--nums", "31337"))
          assertMatch(invoked){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Invalid(
            ArgSig("nums", _, "Int*", _, _, true),
            "--nums",
            _: NumberFormatException
            )
            ))=>
          }

          check(
            Target, routes(5), List("1", "--args", "foo"),
            Result.Success("1--argsfoo")
          )

          assertMatch(parseInvoke(Target, routes(4), List("1", "2", "3", "--nums", "4"))){
            case Result.Error.InvalidArguments(List(
            Result.ParamError.Invalid(
            ArgSig("nums", _, "Int*", _, _, true),
            "--nums",
            _: NumberFormatException
            )
            ))=>
          }
        }

        test("notEnoughNormalArgsStillFails"){
          assertMatch(parseInvoke(Target, routes(5), List())){
            case Result.Error.MismatchedArguments(List(ArgSig("first", _, _, _, _, false)), Nil, Nil, None) =>
          }
        }
        test("multipleVarargParseFailures"){
          assertMatch(parseInvoke(Target, routes(4), List("aa", "bb", "3"))){
            case Result.Error.InvalidArguments(
            List(
              Result.ParamError.Invalid(ArgSig("nums", _, "Int*", _, _, true), "aa", _: NumberFormatException),
              Result.ParamError.Invalid(ArgSig("nums", _, "Int*", _, _, true), "bb", _: NumberFormatException)
            )
            )=>
          }
          assertMatch(parseInvoke(Target, routes(5), List("aa", "bb", "3"))){
            case Result.Error.InvalidArguments(
            List(
              Result.ParamError.Invalid(ArgSig("first", _, "Int", _, _, false), "aa", _: NumberFormatException)
            )
            )=>
          }
        }
      }

      test("failures"){
        test("missingParams"){
          assertMatch(parseInvoke(Target, routes(1), List.empty)){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false)), Nil, Nil, None) =>
          }
          assertMatch(parseInvoke(Target, routes(2), List("--s", "omg"))){
            case Result.Error.MismatchedArguments(List(ArgSig("i", _, _, _, _, false)), Nil, Nil, None) =>
          }
        }
        test("invalidParams") - assertMatch(parseInvoke(Target, routes(1), List("lol"))){
          case Result.Error.InvalidArguments(
          List(Result.ParamError.Invalid(ArgSig("i", _, _, _, _, _), "lol", _))
          ) =>
        }

        test("tooManyParams") - check(
          Target, routes(0), List("1", "2"),
          Result.Error.MismatchedArguments(Nil, List("1", "2"), Nil, None)
        )


        test("redundantParams"){
          val parsed = parseInvoke(Target, routes(2), List("1", "--i", "2"))
          assertMatch(parsed){
            case Result.Error.MismatchedArguments(
              Nil, Nil, Seq((ArgSig("i", _, _, _, _, false), Seq("1", "2"))), None
            ) =>
          }
        }
        test("failing") - check(
          Target,
          routes(3),
          List(),
          Result.Error.Exception(MyException)
        )
      }
    }
  }
}
