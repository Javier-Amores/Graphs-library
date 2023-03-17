package graph

/**
 * A trait representing a directed edge.
 *
 * @tparam V the type of the vertices in the directed edge
 */
trait IsDirectedEdge[+V] {
  /**
   * Returns the source vertex of the directed edge.
   *
   * @return the source vertex of the directed edge
   */
  def source: V

  /**
   * Returns the destination vertex of the directed edge.
   *
   * @return the destination vertex of the directed edge
   */
  def destination: V
}
