package mainargs

import mainargs.Result.Error.{InvalidArguments, MismatchedArguments}
import utest._

object PositionalDisabledTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = false)
      .flatMap(entryPoint.invoke(base, _))
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

      val routes = generateRoutes[Target.type].value

      test("invoke"){
        test - check(
          Target,
          routes(1)
          , List("2"),
          MismatchedArguments(
            List(ArgSig("i",None,"Int",None,None,false,false)),
            List("2"),
            List(),
            None
          )
        )
        test - check(
          Target,
          routes(2),
          List("2"),
          MismatchedArguments(
            List(ArgSig("i",None,"Int",None,None,false,false)),
            List("2"),
            List(),
            None
          )
        )
        test - check(
          Target,
          routes(2),
          List("3", "x"),
          MismatchedArguments(
            List(ArgSig("i",None,"Int",None,None,false,false)),
            List("3", "x"),
            List(),
            None
          )
        )
        test - check(
          Target,
          routes(2),
          List("--i", "3", "x"),
          MismatchedArguments(List(),List("x"),List(),None)
        )
      }
      test("varargs"){
        test("happyPathPasses"){
          test - check(
            Target,
            routes(5),
            List("1", "2", "3", "4", "5"),
            MismatchedArguments(
              List(ArgSig("first",Some('f'),"Int",None,None,false,false)),
              List(),
              List(),
              None
            )
          )
        }
        test("emptyVarargsPasses"){
          test - check(
            Target,
            routes(5),
            List("1"),
            MismatchedArguments(
              List(ArgSig("first",Some('f'),"Int",None,None,false,false)),
              List(),
              List(),
              None
            )
          )
        }
        test("varargsAreAlwaysPositional"){

          test - check(
            Target, routes(5), List("1", "--args", "foo"),
            MismatchedArguments(
              List(ArgSig("first",Some('f'),"Int",None,None,false,false)),
              List(),
              List(),
              None
            )
          )

        }

        test("multipleVarargParseFailures"){
          test - assertMatch(parseInvoke(Target, routes(5), List("aa", "bb", "3"))){
            case MismatchedArguments(
              List(ArgSig("first",Some('f'),"Int",None,None,false,false)),
              List(),
              List(),
              None
            ) =>
          }
        }
      }

      test("failures"){
        test("invalidParams") - assertMatch(parseInvoke(Target, routes(1), List("lol"))){
          case MismatchedArguments(
          List(ArgSig("i",None,"Int",None,None,false,false)),
          List("lol"),
          List(),
          None
          ) =>
        }



        test("redundantParams"){
          val parsed = parseInvoke(Target, routes(2), List("1", "--i", "2"))
          assertMatch(parsed){
            case MismatchedArguments(
              List(ArgSig("i", None,"Int",None,None,false,false)),
              List("1", "--i", "2"),
              List(),
              None
            ) =>
          }
        }
      }
    }
  }
}
