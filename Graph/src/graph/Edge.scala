package graph

// An undirected edge
object Edge {
  def apply[V](vertex1: V, vertex2: V): Edge[V] = new Edge[V](vertex1, vertex2)

  def unapply[V](edge: Edge[V]): Option[(V, V)] = Some(edge.vertex1, edge.vertex2)
}

/**
 * A class representing an undirected, unweighted edge.
 *
 * @param _v1 the first vertex of the edge
 * @param _v2 the second vertex of the edge
 * @tparam V the type of the vertices in the edge
 */
class Edge[+V](protected val _v1: V, protected val _v2: V) extends IsEdge[V] {
  override def vertex1: V = _v1

  override def vertex2: V = _v2

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
