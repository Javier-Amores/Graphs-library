package graph.traversal

import graph.{GraphException, UndirectedGraph}

/**
 * Class for checking graph connectivity using Depth-First Traversal.
 *
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class DFTConnected[V](graph: UndirectedGraph[V]) extends Connected[V](graph) {
  protected val traversal: FirstTraversal[V] = try {
    new DepthFirstTraversal[V](graph, graph.vertices.head)
  }
  catch {
    case empty: NoSuchElementException => throw GraphException("Graph is empty")
  }
}
