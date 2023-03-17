package graph

/**
 * A trait representing an undirected, unweighted graph.
 *
 * @tparam V the type of vertices in the graph
 */
trait UndirectedUnweightedGraph[V] extends UndirectedGraph[V] with UnweightedGraph[V] {
  /**
   * Adds an edge to the graph.
   *
   * @param edge the edge to add
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(edge: Edge[V]): Boolean

  /**
   * Checks if the graph contains the specified edge.
   *
   * @param edge the edge to check for
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(edge: Edge[V]): Boolean

  /**
   * Deletes the specified unweighted edge from the graph.
   *
   * @param edge the edge to delete
   * @return true if the edge was successfully deleted, false otherwise
   */
  def deleteEdge(edge: Edge[V]): Boolean
}