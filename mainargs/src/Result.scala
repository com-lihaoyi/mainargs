package mainargs

/**
 * Represents what comes out of an attempt to invoke an [[Main]].
 * Could succeed with a value, but could fail in many different ways.
 */
sealed trait Result[+T]{
  def map[V](f: T => V): Result[V] = this match{
    case Result.Success(v) => Result.Success(f(v))
    case e: Result.Failure => e
  }
  def flatMap[V](f: T => Result[V]): Result[V] = this match{
    case Result.Success(v) => f(v)
    case e: Result.Failure => e
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
  sealed trait Failure extends Result[Nothing]
  object Failure{
    sealed trait Early extends Failure
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
    case class Exception(t: Throwable) extends Failure
    /**
     * Invoking the [[Main]] failed because the arguments provided
     * did not line up with the arguments expected
     */
    case class MismatchedArguments(missing: Seq[ArgSig.Simple[_, _]] = Nil,
                                   unknown: Seq[String] = Nil,
                                   duplicate: Seq[(ArgSig.Named[_, _], Seq[String])] = Nil,
                                   incomplete: Option[ArgSig.Simple[_, _]] = None) extends Failure
    /**
     * Invoking the [[Main]] failed because there were problems
     * deserializing/parsing individual arguments
     */
    case class InvalidArguments(values: Seq[ParamError]) extends Failure
  }

  sealed trait ParamError
  object ParamError{

    /**
     * Something went wrong trying to de-serialize the input parameter
     */
    case class Failed(arg: ArgSig.Terminal[_, _], tokens: Seq[String], errMsg: String) extends ParamError
    /**
     * Something went wrong trying to de-serialize the input parameter;
     * the thrown exception is stored in [[ex]]
     */
    case class Exception(arg: ArgSig.Terminal[_, _], tokens: Seq[String], ex: Throwable) extends ParamError
    /**
     * Something went wrong trying to evaluate the default value
     * for this input parameter
     */
    case class DefaultFailed(arg: ArgSig.Simple[_, _], ex: Throwable) extends ParamError
  }
}

sealed trait ParamResult[+T]{
  def map[V](f: T => V): ParamResult[V] = this match{
    case ParamResult.Success(v) => ParamResult.Success(f(v))
    case e: ParamResult.Failure => e
  }
  def flatMap[V](f: T => ParamResult[V]): ParamResult[V] = this match{
    case ParamResult.Success(v) => f(v)
    case e: ParamResult.Failure => e
  }
}
object ParamResult{
  case class Failure(errors: Seq[Result.ParamError]) extends ParamResult[Nothing]
  case class Success[T](value: T) extends ParamResult[T]
}