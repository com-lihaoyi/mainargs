package mainargs
import utest._

@main
case class Config(@arg(name = "predef-code",
                    doc = "Any commands you want to execute at the start of the REPL session")
                  predefCode: String = "",
                  @arg(short = 'c',
                    doc = "Pass in code to be run immediately in the REPL")
                  code: Option[String] = None,
                  @arg(short = 'h',
                    doc = "The home directory of the REPL; where it looks for config and caches")
                  home: String = new java.io.File(".").toString,
                  @arg(short = 'p',
                    doc = "Lets you load your predef from a custom location, rather than the default location in your Ammonite home")
                  predef: Option[String] = None,
                  @arg(name = "no-home-predef",
                    doc = "Disables the default behavior of loading predef files from your " +
                      " ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You " +
                      "can choose an additional predef to use using `--predef")
                  homePredef: Boolean = true,
                  @arg(name = "no-default-predef",
                    doc = "Disable the default predef and run Ammonite with the minimal " +
                      "predef possible")
                  defaultPredef: Boolean = true,
                  @arg(name = "silent", short = 's',
                    doc = "Make ivy logs go silent instead of printing though failures " +
                      "will still throw exception")
                  verboseOutput: Boolean = false,
                  @arg(name = "color",
                    doc = "Enable or disable colored output; by default colors are enabled " +
                    "in both REPL and scripts if the console is interactive, and disabled " +
                    "otherwise")
                  colored: Option[Boolean] = None,
                  @arg(short = 'w',
                    doc = "Watch and re-run your scripts when they change")
                  watch: Boolean = false,
                  @arg(doc = "Run a BSP server against the passed scripts")
                  bsp: Boolean = false,
                  @arg(doc = "Hide parts of the core of Ammonite and some of its dependencies. "+
                    "By default, the core of Ammonite and all of its dependencies can "+
                    "be seen by users from the Ammonite session. This option mitigates "+
                    "that via class loader isolation.")
                  thin: Boolean = false,
                  @arg(short = 'b', doc = "Customize the welcome banner that gets shown when Ammonite starts")
                  banner: String = "Welcome to Ammonite!",
                  @arg(name = "class-based",
                    doc = "Wrap user code in classes rather than singletons, "+
                      "typically for Java serialization friendliness.")
                  classBased: Boolean = false)


object AmmoniteTests extends TestSuite{
  def parseInvoke[T](base: T, entryPoint: EntryPoint[T], input: List[String]) = {
    Grouping.groupArgs(input, entryPoint.argSigs, allowPositional = true)
      .flatMap(entryPoint.invoke(base, _))
      .map(_.value)
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

      val routes = generateClassRoute[Config, Config.type]

      test("formatMainMethods"){
        Renderer.formatMainMethodSignature(Config, routes, 0, 95)
      }
      test("parseInvoke"){
        Parser(Array("--code", "println(1)")).constructEither[Config](generateClassRoute[Config]) ==>
          Right(Config(code = Some("println(1)")))
      }
    }
  }
}
