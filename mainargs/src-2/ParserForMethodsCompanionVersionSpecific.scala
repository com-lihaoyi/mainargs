package mainargs

import acyclic.skipped

import scala.language.experimental.macros

private[mainargs] trait ParserForMethodsCompanionVersionSpecific {
  def apply[B](base: B): ParserForMethods[B] = macro Macros.parserForMethods[B]
}
