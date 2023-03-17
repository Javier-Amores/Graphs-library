package graph

/**
 * A trait representing the weight of an edge.
 *
 * @tparam W the type of the weight in the edge
 */
trait IsWeighted[+W] {
  /**
   * Returns the weight of the edge.
   *
   * @return the weight of the edge
   */
  def weight: W
}
