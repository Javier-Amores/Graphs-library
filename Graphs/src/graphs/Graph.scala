package graphs

trait Graph[V]{
  /**
   * Return
   * @return Set[V]
   */
  def vertices: Set[V]

  /**
   * @return
   */
  def edges: Set[Edge[V]]
  def addVertex(node: V): Unit

  /**
   * hola
   * @param nodeSet que hace
   */
  def addVertices(nodeSet: Set[V]): Unit
  def addEdge(source: V ,destination:V): Unit
  def addEdge(edge: Edge[V]): Unit
  def addEdges(edgeSet: Set[Edge[V]]): Unit
  def removeVertex(node: V): Unit
  def removeVertices(nodeSet: Set[V]): Unit

  /**
   * @param edge sad
   */
  def removeEdge(edge: Edge[V]): Unit
  def removeEdges(edgeSet: Set[Edge[V]]): Unit
  def getAdjacentNodes(node: V): Set[V]

  /**
   * the order is defined as the number of nodes in the graph.
   * @return the order of the graph
   */
  def order: Int //the order is defined as the number of nodes in the graph.
  def size: Int //the size is defined as the number of edges in the graph.

  /**
   * @param node
   * @return the number of edges meeting at input node
   */
  def degree(node: V): Int //returns the number of edges meeting at v
  def complete(): Unit //makes the graph complete, i.e, each pair of nodes is connected.
}


