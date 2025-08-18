package mainargs

private[mainargs] trait ParserForMethodsCompanionVersionSpecific {
  inline def apply[B](base: B): ParserForMethods[B] = ${ Macros.parserForMethods[B]('base) }
}
