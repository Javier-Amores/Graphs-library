package graphs

trait Graph[V]{
  /**
   *
   * @return Set of vertices of the graph
   */
  def vertices: Set[V]

  /**
   * @return Set of edges of the graph
   */
  def edges: Set[Edge[V]]

  /**
   * Adds the input node to the graph.
   * @param node A vertex that doesn't belong to the graph yet.
   */
  def addVertex(node: V): Unit

  /**
   * Adds a set of vertices to the graph.
   * @param nodeSet A set of vertices. Each vertex can't belong to the graph.
   */
  def addVertices(nodeSet: Set[V]): Unit

  /**
   * Connect two vertices to make a new edge.
   * @param source A vertex that must belong to the graph.
   * @param destination A vertex that must belong to the graph.
   */
  def addEdge(source: V ,destination:V): Unit

  /**
   * Include a new edge in the graph.
   * @param edge An Edge object. Both source and destination must belong to the graph.
   */
  def addEdge(edge: Edge[V]): Unit

  /**
   * Adds a set of edges to the graph.
   * @param edgeSet A set of Edge type objects.
   */
  def addEdges(edgeSet: Set[Edge[V]]): Unit

  /**
   * Deletes an existing vertex from the graph.
   * @param node A vertex that must belong to the graph.
   */
  def removeVertex(node: V): Unit

  /**
   * Deletes a set of vertices from the graph.
   * @param nodeSet A set of vertices. Each vertex must belong to the graph.
   */
  def removeVertices(nodeSet: Set[V]): Unit

  /**
   * Deletes an existing edge from the graph.
   * @param edge A Edge type object.
   */
  def removeEdge(edge: Edge[V]): Unit

  /**
   * Deletes a set of existing edges from the graph.
   * @param edgeSet A set of Edge type objects.
   */
  def removeEdges(edgeSet: Set[Edge[V]]): Unit

  /**
   * Gives the set of vertices adjacent to a node.
   * @param node An existing vertex.
   * @return The set of vertices adjacent to node.
   */
  def getAdjacentNodes(node: V): Set[V]

  /**
   * the order is defined as the number of nodes in the graph.
   * @return the order of the graph
   */
  def order: Int //the order is defined as the number of nodes in the graph.

  /**
   * The size is defined as the number of edges in the graph.
   * @return the size of the graph.
   */
  def size: Int //the size is defined as the number of edges in the graph.

  /**
   * Gives the number of edges meeting at node.
   * @param node A vertex that must belong to the graph.
   * @return the number of edges meeting at input node.
   */
  def degree(node: V): Int //returns the number of edges meeting at v

  /**
   * Connect each node with each other so that each pair of nodes is connected.
   */
  def complete(): Unit //makes the graph complete, i.e, each pair of nodes is connected.
}


