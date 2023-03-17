package graph

/**
 * A trait representing a weighted graph, with or without directed edges.
 *
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights on the edges in the graph
 */
// any weighted graph, directed or not
trait WeightedGraph[V, W] extends Graph[V, ({type E[X] = IsWeightedEdge[X, W]})#E] {
  /**
   * Adds an edge to the graph with the specified weight, connecting the two specified vertices.
   *
   * @param vertex1 the first vertex to connect
   * @param vertex2 the second vertex to connect
   * @param weight  the weight of the edge to add
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Deletes an edge from the graph with the specified weight.
   *
   * @param vertex1 the first vertex to disconnect
   * @param vertex2 the second vertex to disconnect
   * @param weight  the weight of the edge to delete
   * @return true if the edge was successfully deleted, false otherwise
   */
  def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Checks whether an edge with the specified weight exists between the two specified vertices.
   *
   * @param vertex1 the first vertex to check
   * @param vertex2 the second vertex to check
   * @param weight  the weight of the edge to check
   * @return true if the edge exists, false otherwise
   */
  def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean

  /**
   * Returns the weight of the edge between the two specified vertices, if it exists
   *
   * @param vertex1 the first vertex
   * @param vertex2 the second vertex
   * @return an option containing the weight of the edge if it exists, or None otherwise
   */
  def weightOfEdge(vertex1: V, vertex2: V): Option[W]
}
