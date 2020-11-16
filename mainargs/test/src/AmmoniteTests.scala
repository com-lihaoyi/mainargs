package mainargs
import utest._


object AmmoniteTests extends TestSuite{
  """
Ammonite REPL & Script-Runner, 2.2.0
usage: amm [ammonite-options] [script-file [script-options]]

  --predef-code        Any commands you want to execute at the start of the REPL session
  -c, --code           Pass in code to be run immediately in the REPL
  -h, --home           The home directory of the REPL; where it looks for config and caches
  -p, --predef         Lets you load your predef from a custom location, rather than the
                       default location in your Ammonite home
  --no-home-predef     Disables the default behavior of loading predef files from your
                       ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can
                       choose an additional predef to use using `--predef
  --no-default-predef  Disable the default predef and run Ammonite with the minimal predef
                       possible
  -s, --silent         Make ivy logs go silent instead of printing though failures will
                       still throw exception
  --help               Print this message
  --color              Enable or disable colored output; by default colors are enabled
                       in both REPL and scripts if the console is interactive, and disabled
                       otherwise
  -w, --watch          Watch and re-run your scripts when they change
  --bsp                Run a BSP server against the passed scripts
  --thin               Hide parts of the core of Ammonite and some of its dependencies. By default, the core of
                       Ammonite and all of its dependencies can be seen by users from the Ammonite session. This
                       option mitigates that via class loader isolation.

REPL-specific args:
  -b, --banner         Customize the welcome banner that gets shown when Ammonite starts
  --no-remote-logging  Disable remote logging of the number of times a REPL starts and runs
                       commands
  --class-based        Wrap user code in classes rather than singletons, typically for Java serialization
                       friendliness.
"""
  implicit object PathRead extends ArgParser[os.Path]("path", (prev, s) => Right(os.Path(s, os.pwd)))
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
        "can choose an additional predef to use using `--predef")
    homePredef: Boolean = true,
    @arg(
      name = "no-default-predef",
      doc = "Disable the default predef and run Ammonite with the minimal " +
        "predef possible")
    defaultPredef: Boolean = true,
    @arg(
      name = "silent", short = 's',
      doc = "Make ivy logs go silent instead of printing though failures " +
        "will still throw exception")
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
    @arg(doc = "Run a BSP server against the passed scripts")
    bsp: Boolean = false,
    @arg(doc = "Hide parts of the core of Ammonite and some of its dependencies. "+
      "By default, the core of Ammonite and all of its dependencies can "+
      "be seen by users from the Ammonite session. This option mitigates "+
      "that via class loader isolation.")
    thin: Boolean = false,
    @arg(short = 'b', doc = "Customize the welcome banner that gets shown when Ammonite starts")
    banner: String = "Welcome to Ammonite!",
    @arg(
      name = "class-based",
      doc = "Wrap user code in classes rather than singletons, "+
        "typically for Java serialization friendliness.")
    classBased: Boolean = false
  )

  val tests = Tests {

    val routes = ClassMains.generate[Config].main

    test("formatMainMethods"){
      println(Renderer.formatMainMethodSignature(Config, routes, 0, 95))
    }
    test("parseInvoke"){
      Parser(Array("--code", "println(1)")).constructEither[Config] ==>
        Right(Config(code = Some("println(1)")))
    }
  }
}

