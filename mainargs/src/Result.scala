package mainargs

/**
 * Represents what comes out of an attempt to invoke an [[EntryPoint]].
 * Could succeed with a value, but could fail in many different ways.
 */
sealed trait Result[+T]{
  def map[V](f: T => V): Result[V] = this match{
    case Result.Success(v) => Result.Success(f(v))
    case e: Result.Error => e
  }
  def flatMap[V](f: T => Result[V]): Result[V] = this match{
    case Result.Success(v) => f(v)
    case e: Result.Error => e
  }
}
object Result{

  /**
   * Invoking the [[EntryPoint]] was totally successful, and returned a
   * result
   */
  case class Success[T](value: T) extends Result[T]

  /**
   * Invoking the [[EntryPoint]] was not successful
   */
  sealed trait Error extends Result[Nothing]
  object Error{

    /**
     * Invoking the [[EntryPoint]] failed with an exception while executing
     * code within it.
     */
    case class Exception(t: Throwable) extends Error

    /**
     * Invoking the [[EntryPoint]] failed because the arguments provided
     * did not line up with the arguments expected
     */
    case class MismatchedArguments(missing: Seq[ArgSig[_]],
                                   unknown: Seq[String],
                                   duplicate: Seq[(ArgSig[_], Seq[String])],
                                   incomplete: Option[ArgSig[_]]) extends Error
    /**
     * Invoking the [[EntryPoint]] failed because there were problems
     * deserializing/parsing individual arguments
     */
    case class InvalidArguments(values: Seq[ParamError]) extends Error
  }

  sealed trait ParamError
  object ParamError{
    /**
     * Something went wrong trying to de-serialize the input parameter;
     * the thrown exception is stored in [[ex]]
     */
    case class Invalid(arg: ArgSig[_], value: String, ex: Throwable) extends ParamError
    /**
     * Something went wrong trying to evaluate the default value
     * for this input parameter
     */
    case class DefaultFailed(arg: ArgSig[_], ex: Throwable) extends ParamError
  }
}

