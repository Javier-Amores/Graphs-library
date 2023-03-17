package graph


/**
 * A trait representing an unweighted, directed graph.
 *
 * @tparam V the type of vertices in the graph
 */
trait DirectedUnweightedGraph[V] extends DirectedGraph[V] with UnweightedGraph[V] {

  /**
   * Adds a edge to the graph.
   *
   * @param directedEdge the edge to add
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(directedEdge: DirectedEdge[V]): Boolean


  /**
   * Checks if the graph contains the specified edge.
   *
   * @param directedEdge the edge to check for
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(directedEdge: DirectedEdge[V]): Boolean


  /**
   * Deletes the specified unweighted edge from the graph.
   *
   * @param directedEdge the edge to delete
   * @return true if the edge was successfully deleted, false otherwise
   */
  def deleteEdge(directedEdge: DirectedEdge[V]): Boolean
}
