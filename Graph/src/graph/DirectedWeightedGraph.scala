package graph

/**
 * A trait representing a weighted, directed graph.
 *
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights on the edges in the graph
 */
trait DirectedWeightedGraph[V, W] extends DirectedGraph[V] with Graph[V, ({type E[X] = DirectedWeightedEdge[X, W]})#E]
  with WeightedGraph[V, W] {
  /**
   * Adds a edge to the graph.
   *
   * @param directedWeightedEdge the edge to add
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean

  /**
   * Checks if the graph contains the specified edge.
   *
   * @param directedWeightedEdge the edge to check for
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean

  /**
   * Deletes the specified unweighted edge from the graph.
   *
   * @param directedWeightedEdge the edge to delete
   * @return true if the edge was successfully deleted, false otherwise
   */
  def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean
}
