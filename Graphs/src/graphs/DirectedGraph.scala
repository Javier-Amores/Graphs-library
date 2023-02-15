package graphs

trait DirectedGraph[V] extends Graph[V]{
  def changeDirection(u: V, v: V)//change direction of edge (U,V)
  def changeDirection(edge: Edge[V])
  def transpose(): Unit //Change direction of all edges
  override def complete(): Unit

}