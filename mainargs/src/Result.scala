package mainargs

/**
 * Represents what comes out of an attempt to invoke an [[Main]].
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
   * Invoking the [[Main]] was totally successful, and returned a
   * result
   */
  case class Success[T](value: T) extends Result[T]

  /**
   * Invoking the [[Main]] was not successful
   */
  sealed trait Error extends Result[Nothing]
  object Error{
    sealed trait Early extends Error
    object Early{


      case class NoMainMethodsDetected() extends Early
      case class SubcommandNotSpecified(options: Seq[String]) extends Early
      case class UnableToFindSubcommand(token: String) extends Early
      case class SubcommandSelectionDashes(token: String) extends Early
    }
    /**
     * Invoking the [[Main]] failed with an exception while executing
     * code within it.
     */
    case class Exception(t: Throwable) extends Error
    /**
     * Invoking the [[Main]] failed because the arguments provided
     * did not line up with the arguments expected
     */
    case class MismatchedArguments(missing: Seq[ArgSig[_]] = Nil,
                                   unknown: Seq[String] = Nil,
                                   duplicate: Seq[(ArgSig[_], Seq[String])] = Nil,
                                   incomplete: Option[ArgSig[_]] = None) extends Error
    /**
     * Invoking the [[Main]] failed because there were problems
     * deserializing/parsing individual arguments
     */
    case class InvalidArguments(values: Seq[ParamError]) extends Error
  }

  sealed trait ParamError
  object ParamError{
    /**
     * Something went wrong trying to de-serialize the input parameter
     */
    case class Failed(arg: ArgSig[_], token: String, errMsg: String) extends ParamError
    /**
     * Something went wrong trying to de-serialize the input parameter;
     * the thrown exception is stored in [[ex]]
     */
    case class Exception(arg: ArgSig[_], token: String, ex: Throwable) extends ParamError
    /**
     * Something went wrong trying to evaluate the default value
     * for this input parameter
     */
    case class DefaultFailed(arg: ArgSig[_], ex: Throwable) extends ParamError
  }
}

