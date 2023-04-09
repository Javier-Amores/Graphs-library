package graph.traversal

import graph._

/**
 * Class for checking graph connectivity.
 *
 * @param graph  the graph to be checked
 * @param method a string specifying the traversal algorithm used to check connectivity ("DFT" or "BFT", default is "BFT")
 * @tparam V the type of the vertices in the graph
 */
case class Connected[V](graph: UndirectedGraph[V], method: String = "BFT") {
  private val startVertex = graph.vertices.head
  private val traversal = {
    if (method == "BFT") {
      new BreadthFirstTraversal[V](graph, startVertex)
    }
    else if (method == "DFT") {
      new DepthFirstTraversal[V](graph, startVertex)
    }
    else {
      throw GraphException("Traversal algorithm not found. Valid inputs: \"DFT\" (Depth first traversal), \"BFT\" (Breadth first traversal).")
    }
  }

  /**
   * Determines whether the graph is connected or not.
   *
   * @return true if the graph is connected, false otherwise.
   */
  def isConnected: Boolean = graph.vertices.forall(traversal.isReachable)

}
