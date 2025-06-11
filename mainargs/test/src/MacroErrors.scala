package mainarg

import utest._
import mainargs.{main, arg, ParserForMethods, Flag}
object MacroErrors extends TestSuite {
  object MultipleParameterLists {
    @main
    def method_name(foo: String)(bar: String) = ()
  }
  object NoParameterList {
    @main
    def method_name = ()
  }

  def tests = Tests {
    test("reportMultipleParametersAsError") {
      compileError("ParserForMethods(MultipleParameterLists)").check(
        "",
        "Multiple parameter lists are not supported"
      )
    }

    test("reportNoParameterAsError") {
      compileError("ParserForMethods(NoParameterList)").check(
        "",
        "At least one parameter list must be declared"
      )
    }

  }
}
