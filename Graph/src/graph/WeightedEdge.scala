package graph

object WeightedEdge {
  def apply[V, W](vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] =
    new WeightedEdge[V, W](vertex1, vertex2, weight)

  def unapply[V, W](weightedEdge: WeightedEdge[V, W]): Option[(V, V, W)] =
    Some(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)
}

/**
 * A class representing a weighted edge.
 *
 * @param _v1 the first vertex of the edge
 * @param _v2 the second vertex of the edge
 * @param _w  the weight value of the edge
 * @tparam V the type of the vertices in the edge
 * @tparam W the type of the weight in the weighted edge
 */
class WeightedEdge[+V, +W](override protected val _v1: V, override protected val _v2: V, protected val _w: W)
  extends Edge[V](_v1, _v2) with IsWeightedEdge[V, W] {
  override def weight: W = _w

  override def equals(other: Any): Boolean = other match {
    case that: WeightedEdge[V, W] =>
      (that canEqual this) &&
        (weight == that.weight) &&
        super.equals(that)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[V, W]]

  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + super.hashCode()
    result = prime * result + (if (weight == null) 0 else weight.hashCode())
    result
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex1, $vertex2, $weight)"

}
