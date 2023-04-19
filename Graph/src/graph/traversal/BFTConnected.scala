package graph.traversal

import graph._


/**
 * Class for checking graph connectivity using Breadth-First Traversal.
 *
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class BFTConnected[V](graph: UndirectedGraph[V]) extends Connected[V](graph) {
  protected val traversal: FirstTraversal[V] = try {
    new BreadthFirstTraversal[V](graph, graph.vertices.head)
  }
  catch {
    case empty: NoSuchElementException => throw GraphException("Graph is empty")
  }
}

