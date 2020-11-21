package mainargs
import utest._


object AmmoniteTests extends TestSuite{
  implicit object PathRead extends TokensReader[os.Path]("path", strs => Right(os.Path(strs.head, os.pwd)))
  @main(
    name = "Ammonite REPL & Script-Runner, 2.2.0",
    doc = "usage: amm [ammonite-options] [script-file [script-options]]")
  case class Config(
    @arg(
      name = "predef-code",
      doc = "Any commands you want to execute at the start of the REPL session")
    predefCode: String = "",
    @arg(
      short = 'c',
      doc = "Pass in code to be run immediately in the REPL")
    code: Option[String] = None,
    @arg(
      short = 'h',
      doc = "The home directory of the REPL; where it looks for config and caches")
    home: os.Path = os.pwd,
    @arg(
      short = 'p',
      doc = "Lets you load your predef from a custom location, rather than the "+
        "default location in your Ammonite home")
    predef: Option[os.Path] = None,
    @arg(
      name = "no-home-predef",
      doc = "Disables the default behavior of loading predef files from your " +
        " ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You " +
        "can choose an additional predef to use using `--predef",
      flag = true)
    noHomePredef: Boolean = false,
    @arg(
      name = "no-default-predef",
      doc = "Disable the default predef and run Ammonite with the minimal " +
        "predef possible",
      flag = true)
    defaultPredef: Boolean = false,
    @arg(
      name = "silent", short = 's',
      doc = "Make ivy logs go silent instead of printing though failures " +
        "will still throw exception",
      flag = true)
    verboseOutput: Boolean = false,
    @arg(
      name = "color",
      doc = "Enable or disable colored output; by default colors are enabled " +
        "in both REPL and scripts if the console is interactive, and disabled " +
        "otherwise")
    colored: Option[Boolean] = None,
    @arg(
      short = 'w',
      doc = "Watch and re-run your scripts when they change")
    watch: Boolean = false,
    @arg(doc = "Run a BSP server against the passed scripts", flag = true)
    bsp: Boolean = false,
    @arg(doc = "Hide parts of the core of Ammonite and some of its dependencies. "+
      "By default, the core of Ammonite and all of its dependencies can "+
      "be seen by users from the Ammonite session. This option mitigates "+
      "that via class loader isolation.",
      flag = true)
    thin: Boolean = false,
    @arg(short = 'b', doc = "Customize the welcome banner that gets shown when Ammonite starts")
    banner: String = "Welcome to Ammonite!",
    @arg(
      name = "class-based",
      doc = "Wrap user code in classes rather than singletons, "+
        "typically for Java serialization friendliness.",
      flag = true)
    classBased: Boolean = false
  )
  implicit val parser = ParserForClass[Config]
  val tests = Tests {



    test("formatMainMethods"){
      val rendered = parser.helpText().trim
      val expected =
        """Ammonite REPL & Script-Runner, 2.2.0
          |usage: amm [ammonite-options] [script-file [script-options]]
          |  --predef-code <str>  Any commands you want to execute at the start of the REPL session
          |  -c --code <str>      Pass in code to be run immediately in the REPL
          |  -h --home <path>     The home directory of the REPL; where it looks for config and caches
          |  -p --predef <path>   Lets you load your predef from a custom location, rather than the default
          |                       location in your Ammonite home
          |  --no-home-predef     Disables the default behavior of loading predef files from your
          |                       ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can choose an
          |                       additional predef to use using `--predef
          |  --no-default-predef  Disable the default predef and run Ammonite with the minimal predef possible
          |  -s --silent          Make ivy logs go silent instead of printing though failures will still throw
          |                       exception
          |  --color <bool>       Enable or disable colored output; by default colors are enabled in both REPL
          |                       and scripts if the console is interactive, and disabled otherwise
          |  -w --watch <bool>    Watch and re-run your scripts when they change
          |  --bsp                Run a BSP server against the passed scripts
          |  --thin               Hide parts of the core of Ammonite and some of its dependencies. By default,
          |                       the core of Ammonite and all of its dependencies can be seen by users from
          |                       the Ammonite session. This option mitigates that via class loader isolation.
          |  -b --banner <str>    Customize the welcome banner that gets shown when Ammonite starts
          |  --class-based        Wrap user code in classes rather than singletons, typically for Java
          |                       serialization friendliness.
          |""".stripMargin.trim

      assert(rendered == expected)

    }
    test("parseInvoke"){
      parser.constructEither(Array("--code", "println(1)")) ==>
        Right(Config(code = Some("println(1)")))
    }
  }
}

