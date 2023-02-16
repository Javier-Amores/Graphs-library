package graphs

trait DirectedGraph[V] extends Graph[V]{
  /**
   *
   * @param source
   * @param destination
   */
  def changeDirection(source: V, destination: V)//change direction of edge (U,V)

  /**
   * @param edge
   */
  def changeDirection(edge: Edge[V])

  /**
   * Change direction of all arcs.
   */
  def transpose(): Unit //Change direction of all edges
  override def complete(): Unit

  /**
   * @param source
   * @param destination
   * @return
   */
  def adjacent(source: V, destination: V):Boolean

}