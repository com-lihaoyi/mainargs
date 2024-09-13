# mainargs 0.7.5

MainArgs is a small, dependency-free library for command line argument parsing
in Scala.

MainArgs is used for command-line parsing of the
[Ammonite Scala REPL](http://ammonite.io/) and for user-defined `@main` methods
in its scripts, as well as for command-line parsing for the
[Mill Build Tool](https://github.com/lihaoyi/mill) and for user-defined
`T.command`s.

- [mainargs](#mainargs)
- [Usage](#usage)
  - [Parsing Main Method Parameters](#parsing-main-method-parameters)
    - [runOrExit](#runorexit)
    - [runOrThrow](#runorthrow)
    - [runEither](#runeither)
    - [runRaw](#runraw)
  - [Multiple Main Methods](#multiple-main-methods)
  - [Parsing Case Class Parameters](#parsing-case-class-parameters)
  - [Re-using Argument Sets](#re-using-argument-sets)
  - [Option or Sequence parameters](#option-or-sequence-parameters)
  - [Short Arguments](#short-arguments)
  - [Annotations](#annotations)
    - [@main](#main)
    - [@arg](#arg)
  - [Customization](#customization)
  - [Custom Argument Parsers](#custom-argument-parsers)
  - [Handlings Leftover Arguments](#handlings-leftover-arguments)
    - [Varargs Parameters](#varargs-parameters)
- [Prior Art](#prior-art)
  - [Ammonite & Mill](#ammonite--mill)
  - [Case App](#case-app)
  - [Scopt](#scopt)
- [Changelog](#changelog)
- [Scaladoc](https://javadoc.io/doc/com.lihaoyi/mainargs_2.13/latest/mainargs/index.html)

# Usage

```scala
ivy"com.lihaoyi::mainargs:0.7.5"
```

## Parsing Main Method Parameters

You can parse command line arguments and use them to call a main method via
`ParserForMethods(...)`:

```scala
package testhello
import mainargs.{main, arg, ParserForMethods, Flag}

object Main{
  @main
  def run(@arg(short = 'f', doc = "String to print repeatedly")
          foo: String,
          @arg(doc = "How many times to print string")
          myNum: Int = 2,
          @arg(doc = "Example flag, can be passed without any value to become true")
          bool: Flag) = {
    println(foo * myNum + " " + bool.value)
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash
$ ./mill example.hello -f hello # short name
hellohello false

$ ./mill example.hello --foo hello # long name
hellohello false

$ ./mill example.hello --foo=hello # gflags-style
hellohello false

$ ./mill example.hello --foo "" # set to empty value
 false

$ ./mill example.hello --foo= # gflags-style empty value
 false

$ ./mill example.hello -f x --my-num 3 # camelCase automatically converted to kebab-case
xxx false

$ ./mill example.hello -f hello --my-num 3 --bool # flags
hellohellohello true

$ ./mill example.hello --wrong-flag
Missing argument: --foo <str>
Unknown argument: "--wrong-flag"
Expected Signature: run
  -f --foo <str>  String to print repeatedly
  --my-num <int>  How many times to print string
  --bool          Example flag
```

Setting default values for the method arguments makes them optional, with the
default value being used if an explicit value was not passed in from the
command-line arguments list.

After calling `ParserForMethods(...)` on the `object` containing your `@main`
methods, you can call the following methods to perform the argument parsing and
dispatch:

### runOrExit

Runs the given main method if argument parsing succeeds, otherwise prints out
the help text to standard error and calls `System.exit(1)` to exit the proess

### runOrThrow

Runs the given main method if argument parsing succeeds, otherwise throws an
exception with the help text

### runEither

Runs the given main method if argument parsing succeeds, returning `Right(v: Any)` containing the return value of the main method if it succeeds, or `Left(s: String)` containing the error message if it fails.

### runRaw

Runs the given main method if argument parsing succeeds, returning
`mainargs.Result.Success(v: Any)` containing the return value of the main method
if it succeeds, or `mainargs.Result.Error` if it fails. This gives you the
greatest flexibility to handle the error cases with custom logic, e.g. if you do
not like the default CLI error reporting and would like to write your own.

## Multiple Main Methods

Programs with multiple entrypoints are supported by annotating multiple `def`s
with `@main`. Each entrypoint can have their own set of arguments:

```scala
package testhello2
import mainargs.{main, arg, ParserForMethods, Flag}

object Main{
  @main
  def foo(@arg(short = 'f', doc = "String to print repeatedly")
          foo: String,
          @arg(doc = "How many times to print string")
          myNum: Int = 2,
          @arg(doc = "Example flag")
          bool: Flag) = {
    println(foo * myNum + " " + bool.value)
  }
  @main
  def bar(i: Int,
          @arg(doc = "Pass in a custom `s` to override it")
          s: String  = "lols") = {
    println(s * i)
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash
$ ./mill example.hello2
Need to specify a sub command: foo, bar

$ ./mill example.hello2 foo -f hello
hellohello false

$ ./mill example.hello2 bar -i 10
lolslolslolslolslolslolslolslolslolslols
```

## Parsing Case Class Parameters

If you want to construct a configuration object instead of directly calling a
method, you can do so via `ParserForClass[T]` and `constructOrExit:

```scala
package testclass
import mainargs.{main, arg, ParserForClass, Flag}

object Main{
  @main
  case class Config(@arg(short = 'f', doc = "String to print repeatedly")
                    foo: String,
                    @arg(doc = "How many times to print string")
                    myNum: Int = 2,
                    @arg(doc = "Example flag")
                    bool: Flag)
  def main(args: Array[String]): Unit = {
    val config = ParserForClass[Config].constructOrExit(args)
    println(config)
  }
}
```

```bash
$ ./mill example.caseclass --foo "hello"
Config(hello,2,Flag(false))

$ ./mill example.caseclass
Missing argument: --foo <str>
Expected Signature: apply
  -f --foo <str>  String to print repeatedly
  --my-num <int>  How many times to print string
  --bool          Example flag
```

`ParserForClass[T]` also provides corresponding `constructOrThrow`,
`constructEither`, or `constructRaw` methods for you to handle the error cases
in whichever style you prefer.

## Re-using Argument Sets

You can share arguments between different `@main` methods by defining them in a
`@main case class` configuration object with an implicit `ParserForClass[T]`
defined:

```scala
package testclassarg
import mainargs.{main, arg, ParserForMethods, ParserForClass, Flag}

object Main{
  @main
  case class Config(@arg(short = 'f', doc = "String to print repeatedly")
                    foo: String,
                    @arg(doc = "How many times to print string")
                    myNum: Int = 2,
                    @arg(doc = "Example flag")
                    bool: Flag)
  implicit def configParser = ParserForClass[Config]

  @main
  def bar(config: Config,
          @arg(name = "extra-message")
          extraMessage: String) = {
    println(config.foo * config.myNum + " " + config.bool.value + " " + extraMessage)
  }
  @main
  def qux(config: Config,
          n: Int) = {
    println((config.foo * config.myNum + " " + config.bool.value + "\n") * n)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash

$ ./mill example.classarg bar --foo cow --extra-message "hello world"
cowcow false hello world

$ ./mill example.classarg qux --foo cow --n 5
cowcow false
cowcow false
cowcow false
cowcow false
cowcow false
```

This allows you to re-use common command-line parsing configuration without
needing to duplicate it in every `@main` method in which it is needed. A `@main def` can make use of multiple `@main case class`es, and `@main case class`es can
be nested arbitrarily deeply.

## Option or Sequence parameters

`@main` method parameters can be `Option[T]` or `Seq[T]` types, representing
optional parameters without defaults or repeatable parameters

```scala
package testoptseq
import mainargs.{main, arg, ParserForMethods}

object Main{
  @main
  def runOpt(opt: Option[Int]) = println(opt)

  @main
  def runSeq(seq: Seq[Int]) = println(seq)

  @main
  def runVec(seq: Vector[Int]) = println(seq)

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash
$ ./mill example.optseq runOpt
None

$ ./mill example.optseq runOpt --opt 123
Some(123)

$ ./mill example.optseq runSeq --seq 123 --seq 456 --seq 789
List(123, 456, 789)
```


## Short Arguments

`@main` method arguments that have single-character names are automatically converted
to short arguments, invoked with a single `-` instead of double `--`. The short version
of an argument can also be given explicitly via the `@arg(short = '...')`: 

```scala
object Base {
  @main
  def bools(a: Flag, b: Boolean = false) = println(Seq(a.value, b, c.value))
  
  @main
  def strs(a: Flag, b: String) = println(Seq(a.value, b))
}
```

These can be invoked as normal, for `Flag`s like `-a` or normal arguments that take 
a value like `-b` below:

```bash
$ ./mill example.short bools -a
Seq(true, false)

$ ./mill example.short bools -b true
Seq(false, true)
```

Multiple short arguments can be combined into one `-ab` call:

$ ./mill example.short bools -ab true
Seq(true, true)
```

Short arguments can be combined with their value:

```scala
$ ./mill example.short bools -btrue
Seq(false, true)
```

And you can combine both multiple short arguments as well as the resulting value:

```scala
$ ./mill example.short bools -abtrue
Seq(true, true)
```

Note that when multiple short arguments are combined, whether via `-ab true` or via `-abtrue`,
only the last short argument (in this case `b`) can take a value.

If an `=` is present in the short argument group after the first character, the short
argument group is treated as a key-value pair with the remaining characters after the `=`
passed as the value to the first short argument:

```scala
$ ./mill example.short strs -b=value
Seq(false, value)

$ ./mill example.short strs -a -b=value
Seq(true, value)
```

You can use `-b=` as a shorthand to set the value of `b` to an empty string:

```scala
$ ./mill example.short strs -a -b=
  Seq(true, )
```

If an `=` is present in the short argument group after subsequent character, all characters
except the first are passed to the first short argument. This can be useful for concisely
passing key-value pairs to a short argument:

```scala
$ ./mill example.short strs -a -bkey=value
Seq(true, key=value)
```

```scala
These can also be combined into a single token, with the first non-`Flag` short argument in the
token consuming the subsequent characters as a string (unless the subsequent characters start with
an `=`, which is skipped):

```scala
$ ./mill example.short strs -ab=value
Seq(true, value)

$ ./mill example.short strs -abkey=value 
Seq(true, key=value)
```

## Annotations

The library's annotations and methods support the following parameters to
customize your usage:

### @main

- `name: String`: lets you specify the top-level name of `@main` method you are
  defining. If multiple `@main` methods are provided, this name controls the
  sub-command name in the CLI. If an explicit `name` not passed, both the
  (typically) `camelCase` name of the Scala `def` as well as its `kebab-case` 
  equivalents will be accepted

- `doc: String`: a documentation string used to provide additional information
  about the command. Normally printed below the command name in the help message

### @arg

- `name: String`: lets you specify the long name of a CLI parameter, e.g.
  `--foo`. If an explicit `name` not passed, both the (typically) `camelCase`
  name of the Scala method parameter as well as its `kebab-case`
  equivalents will be accepted

- `short: Char`: lets you specify the short name of a CLI parameter, e.g. `-f`.
  If not given, the argument can only be provided via its long name

- `doc: String`: a documentation string used to provide additional information
  about the command

- `hidden: Boolean`: if `true` this arg will not be included in the rendered help text.

## Customization

Apart from taking the name of the main `object` or config `case class`,
`ParserForMethods` and `ParserForClass` both have methods that support a number
of useful configuration values:

- `allowPositional: Boolean`: allows you to pass CLI arguments "positionally"
  without the `--name` of the parameter being provided, e.g. `./mill example.hello -f hello --my-num 3 --bool` could be called via `./mill example.hello hello 3 --bool`. Defaults to `false`

- `allowRepeats: Boolean`: allows you to pass in a flag multiple times, and
  using the last provided value rather than raising an error. Defaults to
  `false`

- `totalWidth: Int`: how wide to re-format the `doc` strings to when printing
  the help text. Defaults to `100`

- `printHelpOnExit: Boolean`: whether or not to print the full help text when
  argument parsing fails. This can be convenient, but potentially very verbose
  if the list of arguments is long. Defaults to `true`

- `docsOnNewLine: Boolean`: whether to print argument doc-strings on a new line
  below the name of the argument; this may make things easier to read, but at a
  cost of taking up much more vertical space. Defaults to `false`

- `autoprintHelpAndExit: Option[(Int, PrintStream)]`: whether to detect `--help`
  being passed in automatically, and if so where to print the help message and
  what exit code to exit the process with. Defaults t, `Some((0, System.out))`,
  but can be disabled by passing in `None` if you want to handle help text
  manually (e.g. by calling `.helpText` on the parser object)

- `customName`/`customNames` and `customDoc`/`customDocs`: allows you to
  override the main method names and documentation strings at runtime. This
  allows you to work around limitations in the use of the `@main(name = "...", doc = "...")` annotation that only allows simple static strings.

- `sorted: Boolean`: whether to sort the arguments alphabetically in the help text. Defaults to `true`

- `nameMapper: String => Option[String]`: how Scala `camelCase` names are mapping
  to CLI command and flag names. Defaults to translation to `kebab-case`, but
  you can pass in `mainargs.Util.snakeCaseNameMapper` for `snake_case` CLI names
  or `mainargs.Util.nullNameMapper` to disable mapping.

## Custom Argument Parsers

If you want to parse arguments into types that are not provided by the library,
you can do so by defining an implicit `TokensReader[T]` for that type:

```scala
package testcustom
import mainargs.{main, arg, ParserForMethods, TokensReader}

object Main{
  implicit object PathRead extends TokensReader.Simple[os.Path]{
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))
  }

  @main
  def run(from: os.Path, to: os.Path) = {
    println("from: " + from)
    println("to:   " + to)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash
$ ./mill example.custom --from mainargs --to out
from: /Users/lihaoyi/Github/mainargs/mainargs
to:   /Users/lihaoyi/Github/mainargs/out
```

In this example, we define an implicit `PathRead` to teach MainArgs how to parse
`os.Path`s from the [OS-Lib](https://github.com/lihaoyi/os-lib) library.

Note that `read` takes all tokens that were passed to a particular parameter.
Normally this is a `Seq` of length `1`, but if `allowEmpty` is `true` it could
be an empty `Seq`, and if `alwaysRepeatable` is `true` then it could be
arbitrarily long.

You can see the Scaladoc for `TokenReaders.Simple` for other things you can override:

- [mainargs.TokenReaders.Simple](https://javadoc.io/doc/com.lihaoyi/mainargs_2.13/latest/mainargs/TokensReader$$Simple.html)


## Handlings Leftover Arguments

You can use the special `Leftover[T]` type to store any tokens that are
not consumed by other parsers:

```scala
package testvararg
import mainargs.{main, arg, ParserForMethods, Leftover}

object Main{
  @main
  def run(foo: String,
          myNum: Int = 2,
          rest: Leftover[String]) = {
    println(foo * myNum + " " + rest.value)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

```bash
$ ./mill example.vararg --foo bar i am cow
barbar List(i, am, cow)
```

This also works with `ParserForClass`:

```scala
package testvararg2
import mainargs.{main, arg, ParserForClass, Leftover}

object Main{
  @main
  case class Config(foo: String,
                    myNum: Int = 2,
                    rest: Leftover[String])

  def main(args: Array[String]): Unit = {
    val config = ParserForClass[Config].constructOrExit(args)
    println(config)
  }
}
```

```bash
$ ./mill example.vararg2 --foo bar i am cow
Config(bar,2,Leftover(List(i, am, cow)))
```

You can also pass in a different type to `Leftover`, e.g. `Leftover[Int]` or
`Leftover[Boolean]`, if you want to specify that leftover tokens all parse to a
particular type. Any tokens that do not conform to that type will result in an
argument parsing error.

### Varargs Parameters

You can also use `*` "varargs" to define a parameter that takes in the remainder
of the tokens passed to the CLI:

```scala
package testvararg
import mainargs.{main, arg, ParserForMethods, Leftover}

object Main{
  @main
  def run(foo: String,
          myNum: Int,
          rest: String*) = {
    println(foo * myNum + " " + rest.value)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
```

Note that this has a limitation that you cannot then assign default values to
the other parameters of the function, and hence using `Leftover[T]` is
preferable for those cases.

# Prior Art

## Ammonite & Mill

MainArgs grew out of the user-defined `@main` method feature supported by
Ammonite Scala Scripts:

- http://ammonite.io/#ScriptArguments

This implementation was largely copy-pasted into the Mill build tool, to use for
its user-defined `T.command`s. A parallel implementation was used to parse
command-line parameters for Ammonite and Mill themselves.

Now all four implementations have been unified in the MainArgs library, which
both Ammonite and Mill rely heavily upon. MainArgs also provides some additional
features, such as making it easy to define short versions of flags like `-c` via
the `short = '...'` parameter, or re-naming the command line flags via `name = "..."`.

## Case App

MainArgs' support for parsing Scala `case class`es was inspired by Alex
Archambault's `case-app` library:

- https://github.com/alexarchambault/case-app

MainArgs has the following differentiators over `case-app`:

- Support for directly dispatching to `@main` method(s), rather than only
  parsing into `case class`es
- A dependency-free implementation, without pulling in the heavyweight Shapeless
  library.

## Scopt

MainArgs takes a lot of inspiration from the old Scala Scopt library:

- https://github.com/scopt/scopt

Unlike Scopt, MainArgs lets you call `@main` methods or instantiate `case class`es directly, without needing to separately define a `case class` and
parser. This makes it usable with much less boilerplate than Scopt: a single
method annotated with `@main` is all you need to turn your program into a
command-line friendly tool.

# Changelog

## 0.7.5

- Fix reporting of incomplete short args [#162](https://github.com/com-lihaoyi/mainargs/pull/162)
- Add BigDecimal TokensReader [#152](https://github.com/com-lihaoyi/mainargs/pull/152)

## 0.7.4

- Fix issue with binary infinite recursion in binary compatibility forwarders [#156](https://github.com/com-lihaoyi/mainargs/pull/156)

## 0.7.3

- Add missing `nameMapper` argument to `.runOrExit`, make `sort` param on `runEither` return
  `true` for consistency with the docs [#155](https://github.com/com-lihaoyi/mainargs/pull/155)

## 0.7.2

- Various improvements to Scala 3 macros to match Scala 2 implementation [#148](https://github.com/com-lihaoyi/mainargs/pull/148)

## 0.7.1

- Fix detection of `@main` methods inherited from `trait`s in Scala 3.x [#142](https://github.com/com-lihaoyi/mainargs/pull/142)

## 0.7.0

- Support for Scala-Native 0.5.0
- Minimum version of Scala 3.x raised to 3.3.1

## 0.6.3

- Fix usage of `ParserForClass` for `case class`es with more than 22 parameters in Scala 2.x

## 0.6.2

- Make combine short args that fail to parse go through normal leftover-token code paths
  [#112](https://github.com/com-lihaoyi/mainargs/pull/112)

## 0.6.1

- Fix stackoverflow from incorrect binary compatibility shim
  [#107](https://github.com/com-lihaoyi/mainargs/pull/107)

## 0.6.0

- Automatically map `camelCase` Scala method and argument names to `kebab-case`
  CLI commands and flag names, with configurability by passing in custom
  `nameMappers` [#101](https://github.com/com-lihaoyi/mainargs/pull/101)

- Allow short arguments and their values to be combined into a single token 
  [#102](https://github.com/com-lihaoyi/mainargs/pull/102)

## 0.5.4

- Remove unnecessary PPrint dependency

## 0.5.3

- Support GFlags-style `--foo=bar` syntax [#98](https://github.com/com-lihaoyi/mainargs/pull/98)

## 0.5.1

- Fix handling of case class main method parameter default parameters and
  annotations in Scala 3 [#88](https://github.com/com-lihaoyi/mainargs/pull/88)

## 0.5.0

- Remove hard-code support for mainargs.Leftover/Flag/Subparser to support
  alternate implementations [#62](https://github.com/com-lihaoyi/mainargs/pull/62).
  Note that this is a binary-incompatible change, and any custom 
  `mainargs.TokenReader`s you may implement will need to be updated to implement
  the `mainargs.TokenReader.Simple` trait 
  
- Fix argument parsing of flags in the presence of `allowPositional=true`
  [#66](https://github.com/com-lihaoyi/mainargs/pull/66)

## 0.4.0

- Support sorting to args in help text and sort by default
- Various dependency updates
- This release is binary compatible with mainargs 0.3.0

## 0.3.0

- Update all dependencies to latest
- Support for Scala Native on Scala 3

## 0.2.5

- Backport of *Fix usage of `ParserForClass` for `case class`es with more than
  22 parameters with some default values in Scala 2.x (#123)* on top of 0.2.3

## 0.2.3

- Support Scala 3 [#18](https://github.com/com-lihaoyi/mainargs/pull/18)

## 0.2.2

- Fix hygiene of macros [#12](https://github.com/com-lihaoyi/mainargs/pull/12)
- Allow special characters in method names and argument names [#13](https://github.com/com-lihaoyi/mainargs/pull/13)

## 0.2.1

- Scala-Native 0.4.0 support

## 0.1.7

- Add support for `positional=true` flag in `mainargs.arg`, to specify a
  specific argument can only be passed positionally regardless of whether
  `allowPositional` is enabled for the entire parser

- Allow `-` and `--` to be passed as argument values without being treated as
  flags

## 0.1.4

- First release
