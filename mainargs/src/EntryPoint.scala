package mainargs

case class EntryPoints[T](value: Seq[EntryPoint[T]])

/**
 * What is known about a single endpoint for our routes. It has a [[name]],
 * [[argSigs]] for each argument, and a macro-generated [[invoke0]]
 * that performs all the necessary argument parsing and de-serialization.
 *
 * Realistically, you will probably spend most of your time calling [[invoke]]
 * instead, which provides a nicer API to call it that mimmicks the API of
 * calling a Scala method.
 */
case class EntryPoint[T](name: String,
                         argSigs: Seq[ArgSig[T]],
                         doc: Option[String],
                         varargs: Boolean,
                         invoke0: (T, Map[String, String], Seq[String]) => Result[Any]){
  def invoke(target: T, grouped: Grouping[T]): Result[Any] = {
    try invoke0(target, grouped.grouped.map{case (k, b) => (k.name, b)}, grouped.remaining)
    catch{case e: Throwable => Result.Error.Exception(e)}
  }
}

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
                     varargs: Boolean,
                     flag: Boolean)
