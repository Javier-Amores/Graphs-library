package graph

object DirectedWeightedEdge {
  def apply[V, W](source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] =
    new DirectedWeightedEdge[V, W](source, destination, weight)

  def unapply[V, W](directedWeightedEdge: DirectedWeightedEdge[V, W]): Option[(V, V, W)] =
    Some(directedWeightedEdge.source, directedWeightedEdge.destination, directedWeightedEdge.weight)
}

class DirectedWeightedEdge[V, W](source: V, destination: V, val weight: W) extends DirectedEdge[V](source, destination) {
  override def equals(other: Any): Boolean = other match {
    case that: DirectedWeightedEdge[V,W] =>
      (that canEqual this) &&
        (weight == that.weight) &&
        super.equals(that)
    case _ => false
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[DirectedWeightedEdge[V, W]]

  override def hashCode(): Int = {
    super.hashCode() + (if (weight == null) 0 else weight.hashCode())
  }

  override def toString: String = s"${getClass.getSimpleName}($source, $destination, $weight)"
}
