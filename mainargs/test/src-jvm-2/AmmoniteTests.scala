package mainargs

import utest._

object AmmoniteDefaults {
  val welcomeBanner = {
    def scalaVersion = scala.util.Properties.versionNumberString

    def javaVersion = System.getProperty("java.version")

    s"Welcome to the Ammonite Repl (Scala $scalaVersion Java $javaVersion)"
  }

  def ammoniteHome = os.Path(System.getProperty("user.home")) / ".ammonite"
}

@main
case class AmmoniteConfig(
    core: AmmoniteConfig.Core,
    predef: AmmoniteConfig.Predef,
    repl: AmmoniteConfig.Repl,
    rest: Leftover[String]
)

object AmmoniteConfig {
  implicit object PathRead extends TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  case class InjectedConstant()

  implicit object InjectedTokensReader extends TokensReader.Constant[InjectedConstant] {
    def read() = Right(new InjectedConstant())
  }
  @main
  case class Core(
      injectedConstant: InjectedConstant,
      @arg(
        name = "no-default-predef",
        doc = "Disable the default predef and run Ammonite with the minimal predef possible"
      )
      noDefaultPredef: Flag,
      @arg(
        short = 's',
        doc =
          "Make ivy logs go silent instead of printing though failures will " +
            "still throw exception"
      )
      silent: Flag,
      @arg(
        short = 'w',
        doc = "Watch and re-run your scripts when they change"
      )
      watch: Flag,
      @arg(doc = "Run a BSP server against the passed scripts")
      bsp: Flag,
      @arg(
        short = 'c',
        doc = "Pass in code to be run immediately in the REPL"
      )
      code: Option[String] = None,
      @arg(
        short = 'h',
        doc = "The home directory of the REPL; where it looks for config and caches"
      )
      home: os.Path = AmmoniteDefaults.ammoniteHome,
      @arg(
        name = "predef",
        short = 'p',
        doc =
          "Lets you load your predef from a custom location, rather than the " +
            "default location in your Ammonite home"
      )
      predefFile: Option[os.Path] = None,
      @arg(
        doc =
          "Enable or disable colored output; by default colors are enabled " +
            "in both REPL and scripts if the console is interactive, and disabled " +
            "otherwise"
      )
      color: Option[Boolean] = None,
      @arg(
        doc =
          "Hide parts of the core of Ammonite and some of its dependencies. By default, " +
            "the core of  Ammonite and all of its dependencies can be seen by users from the " +
            "Ammonite session. This option mitigates that via class loader isolation."
      )
      thin: Flag,
      @arg(doc = "Print this message")
      help: Flag
  )
  implicit val coreParser = ParserForClass[Core]

  @main
  case class Predef(
      @arg(
        name = "predef-code",
        doc = "Any commands you want to execute at the start of the REPL session"
      )
      predefCode: String = "",
      @arg(
        name = "no-home-predef",
        doc =
          "Disables the default behavior of loading predef files from your " +
            "~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can " +
            "choose an additional predef to use using `--predef"
      )
      noHomePredef: Flag
  )
  implicit val predefParser = ParserForClass[Predef]

  @main
  case class Repl(
      @arg(
        short = 'b',
        doc = "Customize the welcome banner that gets shown when Ammonite starts"
      )
      banner: String = AmmoniteDefaults.welcomeBanner,
      @arg(
        name = "no-remote-logging",
        doc =
          "(deprecated) Disable remote logging of the number of times a REPL starts and runs commands"
      )
      noRemoteLogging: Flag,
      @arg(
        doc =
          "Wrap user code in classes rather than singletons, typically for Java serialization " +
            "friendliness."
      )
      classBased: Flag
  )
  implicit val replParser = ParserForClass[Repl]
}

object AmmoniteTests extends TestSuite {
  val parser = ParserForClass[AmmoniteConfig]
  val tests = Tests {

    test("formatMainMethods.unsorted") {
      val customName = s"Ammonite REPL & Script-Runner"
      val customDoc = "usage: amm [ammonite-options] [script-file [script-options]]"
      val rendered =
        parser.helpText(customName = customName, customDoc = customDoc, sorted = false).trim
      val expected =
        """Ammonite REPL & Script-Runner
          |usage: amm [ammonite-options] [script-file [script-options]]
          |  --no-default-predef  Disable the default predef and run Ammonite with the minimal predef possible
          |  -s --silent          Make ivy logs go silent instead of printing though failures will still throw
          |                       exception
          |  -w --watch           Watch and re-run your scripts when they change
          |  --bsp                Run a BSP server against the passed scripts
          |  -c --code <str>      Pass in code to be run immediately in the REPL
          |  -h --home <path>     The home directory of the REPL; where it looks for config and caches
          |  -p --predef <path>   Lets you load your predef from a custom location, rather than the default
          |                       location in your Ammonite home
          |  --color <bool>       Enable or disable colored output; by default colors are enabled in both REPL
          |                       and scripts if the console is interactive, and disabled otherwise
          |  --thin               Hide parts of the core of Ammonite and some of its dependencies. By default,
          |                       the core of Ammonite and all of its dependencies can be seen by users from
          |                       the Ammonite session. This option mitigates that via class loader isolation.
          |  --help               Print this message
          |  --predef-code <str>  Any commands you want to execute at the start of the REPL session
          |  --no-home-predef     Disables the default behavior of loading predef files from your
          |                       ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can choose an
          |                       additional predef to use using `--predef
          |  -b --banner <str>    Customize the welcome banner that gets shown when Ammonite starts
          |  --no-remote-logging  (deprecated) Disable remote logging of the number of times a REPL starts and
          |                       runs commands
          |  --classBased         Wrap user code in classes rather than singletons, typically for Java
          |                       serialization friendliness.
          |  rest <str>...
          |""".stripMargin.trim

      assert(rendered == expected)

    }

    test("formatMainMethods.sorted") {
      val customName = s"Ammonite REPL & Script-Runner"
      val customDoc = "usage: amm [ammonite-options] [script-file [script-options]]"
      val rendered =
        parser.helpText(customName = customName, customDoc = customDoc, sorted = true).trim
      val expected =
        """Ammonite REPL & Script-Runner
          |usage: amm [ammonite-options] [script-file [script-options]]
          |  -b --banner <str>    Customize the welcome banner that gets shown when Ammonite starts
          |  --bsp                Run a BSP server against the passed scripts
          |  -c --code <str>      Pass in code to be run immediately in the REPL
          |  --classBased         Wrap user code in classes rather than singletons, typically for Java
          |                       serialization friendliness.
          |  --color <bool>       Enable or disable colored output; by default colors are enabled in both REPL
          |                       and scripts if the console is interactive, and disabled otherwise
          |  -h --home <path>     The home directory of the REPL; where it looks for config and caches
          |  --help               Print this message
          |  --no-default-predef  Disable the default predef and run Ammonite with the minimal predef possible
          |  --no-home-predef     Disables the default behavior of loading predef files from your
          |                       ~/.ammonite/predef.sc, predefScript.sc, or predefShared.sc. You can choose an
          |                       additional predef to use using `--predef
          |  --no-remote-logging  (deprecated) Disable remote logging of the number of times a REPL starts and
          |                       runs commands
          |  -p --predef <path>   Lets you load your predef from a custom location, rather than the default
          |                       location in your Ammonite home
          |  --predef-code <str>  Any commands you want to execute at the start of the REPL session
          |  -s --silent          Make ivy logs go silent instead of printing though failures will still throw
          |                       exception
          |  --thin               Hide parts of the core of Ammonite and some of its dependencies. By default,
          |                       the core of Ammonite and all of its dependencies can be seen by users from
          |                       the Ammonite session. This option mitigates that via class loader isolation.
          |  -w --watch           Watch and re-run your scripts when they change
          |  rest <str>...
          |""".stripMargin.trim

      assert(rendered == expected)

    }

    test("parseInvoke") {
      parser.constructEither(Array("--code", "println(1)").toIndexedSeq) ==>
        Right(
          AmmoniteConfig(
            AmmoniteConfig.Core(
              injectedConstant = AmmoniteConfig.InjectedConstant(),
              noDefaultPredef = Flag(),
              silent = Flag(),
              watch = Flag(),
              bsp = Flag(),
              code = Some("println(1)"),
              thin = Flag(),
              help = Flag()
            ),
            AmmoniteConfig.Predef(noHomePredef = Flag()),
            AmmoniteConfig.Repl(noRemoteLogging = Flag(), classBased = Flag()),
            Leftover()
          )
        )
    }
  }
}
