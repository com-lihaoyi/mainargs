// package mainargs
// import utest._


// object MillTests extends TestSuite{

//  implicit object PathRead extends TokensReader[os.Path]("path", strs => Right(os.Path(strs.head, os.pwd)))
//  @main(
//    name = "Mill Build Tool",
//    doc = "usage: mill [mill-options] [target [target-options]]")
//  case class Config(
//     @arg(
//      doc = "Run Mill in interactive mode and start a build REPL. In this mode, no mill server will be used. Must be the first argument.")
//    repl: Flag = Flag(),
//    @arg(
//      name = "no-server",
//      doc = "Run Mill in interactive mode, suitable for opening REPLs and taking user input. In this mode, no mill server will be used. Must be the first argument.")
//    noServer: Flag = Flag(),
//    @arg(
//      short = 'i',
//      doc = "Run Mill in interactive mode, suitable for opening REPLs and taking user input. In this mode, no mill server will be used. Must be the first argument.")
//    interactive: Flag = Flag(),
//    @arg(
//      short = 'v',
//      doc = "Show mill version and exit.")
//    version: Flag = Flag(),
//    @arg(
//      name = "bell",
//      short = 'b',
//      doc = "Ring the bell once if the run completes successfully, twice if it fails.")
//    ringBell: Flag = Flag(),
//    @arg(
//      name = "disable-ticker",
//      doc = "Disable ticker log (e.g. short-lived prints of stages and progress bars)")
//    disableTicker: Flag = Flag(),
//    @arg(
//      short = 'd',
//      doc = "Show debug output on STDOUT")
//    debug: Flag = Flag(),
//    @arg(
//      name = "keep-going",
//      short = 'k',
//      doc = "Continue build, even after build failures")
//    keepGoing: Flag = Flag(),
//    @arg(
//      name = "define",
//      short = 'D',
//      doc = "Define (or overwrite) a system property")
//    extraSystemProperties: Map[String, String] = Map(),
//    @arg(name = "jobs",
//      short = 'j',
//      doc = "Allow processing N targets in parallel. Use 1 to disable parallel and 0 to use as much threads as available processors.")
//    threadCount: Int = 1,
//    ammoniteConig: AmmoniteTests.Config = AmmoniteTests.Config()
//  )

//  val tests = Tests {

//    val parser = ParserForClass[Config]

//    test("formatMainMethods"){
//      val rendered = parser.helpText()
//      val expected =
//        """Mill Build Tool
//          |usage: mill [mill-options] [target [target-options]]
//          |  --repl               Run Mill in interactive mode and start a build REPL. In this mode, no mill
//          |                       server will be used. Must be the first argument.
//          |  --no-server          Run Mill in interactive mode, suitable for opening REPLs and taking user
//          |                       input. In this mode, no mill server will be used. Must be the first argument.
//          |  -i --interactive     Run Mill in interactive mode, suitable for opening REPLs and taking user
//          |                       input. In this mode, no mill server will be used. Must be the first argument.
//          |  -v --version         Show mill version and exit.
//          |  -b --bell            Ring the bell once if the run completes successfully, twice if it fails.
//          |  --disable-ticker     Disable ticker log (e.g. short-lived prints of stages and progress bars)
//          |  -d --debug           Show debug output on STDOUT
//          |  -k --keep-going      Continue build, even after build failures
//          |  -D --define <k=v>    Define (or overwrite) a system property
//          |  -j --jobs <int>      Allow processing N targets in parallel. Use 1 to disable parallel and 0 to
//          |                       use as much threads as available processors.
//          |  --predef-code <str>  Any commands you want to execute at the start of the REPL session
//          |  -c --code <str>      Pass in code to be run immediately in the REPL
//          |  -h --home <path>     The home directory of the REPL; where it looks for config and caches
//          |  -p --predef <path>   Lets you load your predef from a custom location, rather than the default
//          |                       location in your Ammonite home
//          |  --no-home-predef     Disables the default behavior of loading predef files from your
//          |                       ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can choose an
//          |                       additional predef to use using `--predef
//          |  --no-default-predef  Disable the default predef and run Ammonite with the minimal predef possible
//          |  -s --silent          Make ivy logs go silent instead of printing though failures will still throw
//          |                       exception
//          |  --color <bool>       Enable or disable colored output; by default colors are enabled in both REPL
//          |                       and scripts if the console is interactive, and disabled otherwise
//          |  -w --watch <bool>    Watch and re-run your scripts when they change
//          |  --bsp                Run a BSP server against the passed scripts
//          |  --thin               Hide parts of the core of Ammonite and some of its dependencies. By default,
//          |                       the core of Ammonite and all of its dependencies can be seen by users from
//          |                       the Ammonite session. This option mitigates that via class loader isolation.
//          |  -b --banner <str>    Customize the welcome banner that gets shown when Ammonite starts
//          |  --class-based        Wrap user code in classes rather than singletons, typically for Java
//          |                       serialization friendliness.
//          |""".stripMargin
//      assert(rendered == expected)
//    }
//    test("parseInvoke"){
//      parser.constructEither(Array("--jobs", "12")) ==>
//        Right(Config(threadCount = 12))
//    }
//  }
// }

