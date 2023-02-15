package graphs

final case class GraphException(private val message: String = "", private val cause: Throwable = None.orNull) extends Exception(message, cause)
