package mainargs

import scala.language.experimental.macros

private[mainargs] trait ParserForClassCompanionVersionSpecific {
  inline def apply[T]: ParserForClass[T] = ${ Macros.parserForClass[T] }
}
