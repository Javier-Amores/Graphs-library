package graphs

trait DirectedGraph[V] extends Graph[V]{
  /**
   * Change direction of edge (source, destination)
   * @param source source node
   * @param destination destination node
   */
  def changeDirection(source: V, destination: V)//change direction of edge (U,V)

  /**
   * Change direction of edge
   * @param edge An edge in the graph.
   */
  def changeDirection(edge: Edge[V])

  /**
   * Change direction of all arcs.
   */
  def transpose(): Unit //Change direction of all edges
  override def complete(): Unit

  /**
   *
   * @param source source node
   * @param destination destination node
   * @return true if source and destination are connected, false otherwise.
   */
  def adjacent(source: V, destination: V):Boolean

}