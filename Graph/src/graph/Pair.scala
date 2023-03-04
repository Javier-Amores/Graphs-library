package graph

/**
 * Class for implementing weighted graphs.
 *
 * @param vertex a vertex
 * @param weight a weight
 * @tparam V the type of the vertex
 * @tparam W the type of the weight
 */
// class for implementing weighted graphs
private[graph] case class Pair[V, W](vertex: V, weight: W) {
  // only successor component is used for determining if two Pairs are equal when
  // one any of weights is null
  override def equals(other: Any): Boolean = other match {
    case that: Pair[?, ?] =>
      (that canEqual this) && vertex == that.vertex &&
        (weight == null || that.weight == null || weight == that.weight)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Pair[?, ?]]

  // as equals only depends on successor so has to do hashCode
  override def hashCode(): Int = {
    vertex.hashCode()
  }

  override def toString: String = s"${getClass.getSimpleName}($vertex, $weight)"
}
