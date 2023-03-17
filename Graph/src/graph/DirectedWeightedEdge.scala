package graph

object DirectedWeightedEdge {
  def apply[V, W](source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] =
    new DirectedWeightedEdge[V, W](source, destination, weight)

  def unapply[V, W](directedWeightedEdge: DirectedWeightedEdge[V, W]): Option[(V, V, W)] =
    Some(directedWeightedEdge.source, directedWeightedEdge.destination, directedWeightedEdge.weight)
}


/**
 * A class Representing a directed weighted edge.
 *
 * @param _src the source vertex of the edge
 * @param _dst the destination vertex of the edge
 * @param _w   the weight value of the edge
 * @tparam V the type of the vertices in the edge
 * @tparam W the type of the weight in the weighted edge
 */
class DirectedWeightedEdge[+V, +W](override protected val _src: V, override protected val _dst: V, protected val _w: W)
  extends DirectedEdge[V](_src, _dst) with IsWeightedEdge[V, W] with IsDirectedWeightedEdge[V, W] {
  override def source: V = _src

  override def weight: W = _w

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
