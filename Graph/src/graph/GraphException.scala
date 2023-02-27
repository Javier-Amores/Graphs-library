package graph

/**
 * An exception class for graph related errors.
 * @param message a detailed error message explaining the cause of the exception
 * @param cause an optional cause for this exception
 */
final case class GraphException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)
