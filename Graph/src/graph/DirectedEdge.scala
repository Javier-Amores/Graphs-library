package graph

object DirectedEdge {
  def apply[V](source: V, destination: V): DirectedEdge[V] = new DirectedEdge[V](source, destination)

  def unapply[V](directedEdge: DirectedEdge[V]): Option[(V, V)] = Some(directedEdge.source, directedEdge.destination)
}

/**
 * Represents a directed edge connecting two vertices.
 *
 * @param source      the source vertex
 * @param destination the destination vertex
 * @tparam V the type of vertices in the edge
 */
class DirectedEdge[V](val source: V, val destination: V) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectedEdge[V] =>
      (that canEqual this) &&
        (source == that.source && destination == that.destination)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedEdge[V]]

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + source.hashCode()
    result = prime * result + destination.hashCode()
    result
  }

  override def toString: String = s"${getClass.getSimpleName}($source, $destination)"
}