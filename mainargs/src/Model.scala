
package mainargs

import scala.annotation.{ClassfileAnnotation, StaticAnnotation}

class arg(val name: String = null, val short: Char = 0, val doc: String = null) extends ClassfileAnnotation

class main(val name: String = null, val doc: String = null) extends ClassfileAnnotation


/**
 * Models what is known by the router about a single argument: that it has
 * a [[name]], a human-readable [[typeString]] describing what the type is
 * (just for logging and reading, not a replacement for a `TypeTag`) and
 * possible a function that can compute its default value
 */
case class ArgSig[T](name: String,
                     shortName: Option[Char],
                     typeString: String,
                     doc: Option[String],
                     default: Option[T => Any],
                     varargs: Boolean)

