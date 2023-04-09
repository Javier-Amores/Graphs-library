package graph.traversal

import graph._


/**
 * Class for generating connected components.
 *
 * @param graph  the graph to be studied
 * @param method a string specifying the traversal algorithm used to check connectivity ("DFT" or "BFT", default is "BFT")
 * @tparam V the type of the vertices in the graph
 */
case class ConnectedComponent[V](graph: UndirectedGraph[V], method: String = "BFT") {

  /**
   * Create a new traversal object with the specified starting vertex and traversal method.
   *
   * @param startVertex the starting vertex of the traversal
   * @return a new traversal object
   */
  private def traversal(startVertex: V): Traversal[V] = {
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
   * Get the set of vertices in the same connected component as the specified vertex.
   *
   * @param vertex the vertex to find the connected component of
   * @return a set of vertices in the same connected component
   */
  def get(vertex: V): Set[V] = {
    val connectedComponent = graph.vertices.collect { case currentVertex if traversal(vertex).isReachable(currentVertex) => currentVertex }
    connectedComponent

  }

  /**
   * Get a set of sets, where each set represents a connected component in the graph.
   *
   * @return a set of sets representing connected components in the graph
   */
  def get(): Set[Set[V]] = {
    var vertices: Set[V] = graph.vertices
    var connectedComponents = Set[Set[V]]()
    var currentComponent = Set[V]()
    var currentVertex = null.asInstanceOf[V]
    var currentTraversal = null.asInstanceOf[Traversal[V]]
    while (vertices.nonEmpty) {
      currentVertex = vertices.head
      currentTraversal = traversal(currentVertex)
      currentComponent = {
        vertices.collect { case vertex if currentTraversal.isReachable(vertex) => vertex }
      }
      connectedComponents += currentComponent
      vertices = vertices.diff(currentComponent)
    }
    connectedComponents
  }

  /**
   * Get the number of connected components in the graph.
   *
   * @return the number of connected components in the graph
   */
  def getNumber: Int = get().size


}
