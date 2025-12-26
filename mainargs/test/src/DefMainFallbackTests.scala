package mainargs
import utest._

object DefMainFallbackTests extends TestSuite {

  object UniqueDefMain {
    def main(
        @arg(short = 'f', doc = "String to print")
        foo: String,
        @arg(doc = "Number to multiply")
        num: Int = 2
    ) = {
      foo * num
    }
  }

  object MultipleDefMain {
    def main(foo: String) = foo
    def main(foo: String, bar: String) = foo + bar
  }

  object AnnotatedOverDefMain {
    @main
    def run(foo: String) = s"run: $foo"

    def main(bar: String) = s"main: $bar"
  }

  object OnlyAnnotated {
    @main
    def run(foo: String) = s"run: $foo"
  }

  // Standard JVM main should be ignored, so the custom main should be picked
  object WithStandardJvmMain {
    def main(args: Array[String]): Unit = ()
    def main(foo: String, bar: Int) = s"$foo * $bar"
  }

  // Only has standard JVM main, so no entrypoint should be found
  object OnlyStandardJvmMain {
    def main(args: Array[String]): Unit = println("hello")
  }

  val tests = Tests {
    test("uniqueDefMain") {
      test("basic") {
        Parser(UniqueDefMain).runOrThrow(Array("--foo", "bar")) ==>
          "barbar"
      }
      test("withArg") {
        Parser(UniqueDefMain).runOrThrow(Array("--foo", "bar", "--num", "3")) ==>
          "barbarbar"
      }
      test("shortArg") {
        Parser(UniqueDefMain).runOrThrow(Array("-f", "baz")) ==>
          "bazbaz"
      }
      test("positional") {
        Parser(UniqueDefMain).runOrThrow(Array("qux", "4"), allowPositional = true) ==>
          "quxquxquxqux"
      }
    }

    test("multipleDefMain") {
      // When there are multiple methods named "main", a compile error should be raised
      val error = compileError("Parser(MultipleDefMain)")
      assert(error.msg.contains("Multiple methods named 'main' found"))
      assert(error.msg.contains("@mainargs.main"))
    }

    test("annotatedOverDefMain") {
      // @main annotated methods should take precedence over def main
      val parser = Parser(AnnotatedOverDefMain)
      assert(parser.mains.value.size == 1)
      assert(parser.mains.value.head.name == "run")
      Parser(AnnotatedOverDefMain).runOrThrow(Array("--foo", "test")) ==>
        "run: test"
    }

    test("onlyAnnotated") {
      // If @main annotations exist, they should be used (existing behavior)
      Parser(OnlyAnnotated).runOrThrow(Array("--foo", "test")) ==>
        "run: test"
    }

    test("withStandardJvmMain") {
      // Standard JVM main(args: Array[String]) should be ignored
      val parser = Parser(WithStandardJvmMain)
      assert(parser.mains.value.size == 1)
      Parser(WithStandardJvmMain).runOrThrow(Array("--foo", "hello", "--bar", "3")) ==>
        "hello * 3"
    }

    test("onlyStandardJvmMain") {
      // If only standard JVM main exists, no entrypoint should be found
      val parser = Parser(OnlyStandardJvmMain)
      assert(parser.mains.value.isEmpty)
    }
  }
}
