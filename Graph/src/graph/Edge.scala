package graph

// An undirected edge
object Edge {
  def apply[V](vertex1: V, vertex2: V): Edge[V] = new Edge[V](vertex1, vertex2)

  def unapply[V](edge: Edge[V]): Option[(V, V)] = Some(edge.vertex1, edge.vertex2)
}

/**
 * Represents an edge connecting two vertices.
 * @param vertex1 one vertex of the edge
 * @param vertex2 the other vertex of the edge
 * @tparam V the type of vertices in the edge
 */
class Edge[V](val vertex1: V, val vertex2: V) {
  override def equals(other: Any): Boolean = other match {
    case that: Edge[V] =>
      (that canEqual this) &&
        (vertex1 == that.vertex1 && vertex2 == that.vertex2) ||
        (vertex1 == that.vertex2 && vertex2 == that.vertex1)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[Edge[V]]

  override def hashCode(): Int = {
    vertex1.hashCode() + vertex2.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex1, $vertex2)"
}
