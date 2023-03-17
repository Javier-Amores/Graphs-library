package graph

/**
 * A trait representing an edge.
 *
 * @tparam V the type of the vertices in the edge
 */
trait IsEdge[+V] {
  /**
   * Returns the first vertex of the edge.
   *
   * @return the first vertex of the edge
   */
  def vertex1: V

  /**
   * Returns the second vertex of the edge.
   *
   * @return the second vertex of the edge
   */
  def vertex2: V
}