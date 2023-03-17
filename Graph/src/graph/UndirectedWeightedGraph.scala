package graph

/**
 * A trait representing an undirected, weighted graph.
 *
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights on the edges in the graph
 */
trait UndirectedWeightedGraph[V, W] extends UndirectedGraph[V] with Graph[V, ({type E[X] = WeightedEdge[X, W]})#E]
  with WeightedGraph[V, W] {
  /**
   * Adds a edge to the graph.
   *
   * @param weightedEdge the edge to add
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(weightedEdge: WeightedEdge[V, W]): Boolean

  /**
   * Checks if the graph contains the specified edge.
   *
   * @param weightedEdge the edge to check for
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean

  /**
   * Deletes the specified unweighted edge from the graph.
   *
   * @param weightedEdge the edge to delete
   * @return true if the edge was successfully deleted, false otherwise
   */
  def deleteEdge(weightedEdge: WeightedEdge[V, W]): Boolean
}
