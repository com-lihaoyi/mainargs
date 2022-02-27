package mainargs

import acyclic.skipped

import scala.language.experimental.macros

private [mainargs] trait ParserForClassCompanionVersionSpecific {
  def apply[T]: ParserForClass[T] = macro Macros.parserForClass[T]
}
