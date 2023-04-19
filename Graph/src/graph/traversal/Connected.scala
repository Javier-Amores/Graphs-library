package graph.traversal

import graph.UndirectedGraph

/**
 * Class for checking graph connectivity.
 *
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
abstract class Connected[V](graph: UndirectedGraph[V]) {

  protected val traversal: FirstTraversal[V]

  /**
   * Checks if the graph is connected.
   *
   * @return True if the graph is connected, false otherwise.
   */
  def isConnected: Boolean = graph.vertices.forall(traversal.isReachable)

}
