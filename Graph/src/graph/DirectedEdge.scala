package graph

object DirectedEdge {
  def apply[V](source: V, destination: V): DirectedEdge[V] = new DirectedEdge[V](source, destination)

  def unapply[V](directedEdge: DirectedEdge[V]): Option[(V, V)] = Some(directedEdge.source, directedEdge.destination)
}


/**
 * A class representing a directed edge.
 *
 * @param _src the source vertex of the edge
 * @param _dst the destination vertex of the edge
 * @tparam V the type of the vertices in the edge
 */
class DirectedEdge[+V](protected val _src: V, protected val _dst: V) extends IsEdge[V] with IsDirectedEdge[V] {

  override def vertex1: V = _src

  override def vertex2: V = _dst


  override def source: V = _src

  override def destination: V = _dst

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
