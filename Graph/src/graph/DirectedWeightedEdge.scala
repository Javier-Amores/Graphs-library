package graph

object DirectedWeightedEdge {
  def apply[V, W](source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] =
    new DirectedWeightedEdge[V, W](source, destination, weight)

  def unapply[V, W](directedWeightedEdge: DirectedWeightedEdge[V, W]): Option[(V, V, W)] =
    Some(directedWeightedEdge.source, directedWeightedEdge.destination, directedWeightedEdge.weight)
}

/**
 * Represents a directed weighted edge from a source vertex to a destination vertex with a weight.
 *
 * @param source      the source vertex
 * @param destination the destination vertex
 * @param weight      the weight of the edge
 * @tparam V the type of vertices in the edge
 * @tparam W the type of the weight of the edge
 */
class DirectedWeightedEdge[V, W](source: V, destination: V, val weight: W) extends DirectedEdge[V](source, destination) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectedWeightedEdge[V, W] =>
      (that canEqual this) &&
        (weight == that.weight) &&
        super.equals(that)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedWeightedEdge[V, W]]

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + source.hashCode()
    result = prime * result + destination.hashCode()
    result = prime * result + (if (weight == null) 0 else weight.hashCode())
    result
  }

  override def toString: String = s"${getClass.getSimpleName}($source, $destination, $weight)"
}
