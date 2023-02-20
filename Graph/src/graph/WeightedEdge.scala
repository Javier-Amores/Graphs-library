package graph

object WeightedEdge {
  def apply[V, W](vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] =
    new WeightedEdge[V, W](vertex1, vertex2, weight)

  def unapply[V, W](weightedEdge: WeightedEdge[V, W]): Option[(V, V, W)] =
    Some(weightedEdge.vertex1, weightedEdge.vertex2, weightedEdge.weight)
}

// A weighted undirected edge.
class WeightedEdge[V, W](vertex1: V, vertex2: V, val weight: W) extends Edge[V](vertex1, vertex2) {
  override def equals(other: Any): Boolean = other match {
    case that: WeightedEdge[V,W] =>
        (that canEqual this) &&
        (weight == that.weight) &&
        super.equals(that)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[V, W]]

  override def hashCode(): Int = {
    super.hashCode() + (if (weight == null) 0 else weight.hashCode())
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex1, $vertex2, $weight)"
}

