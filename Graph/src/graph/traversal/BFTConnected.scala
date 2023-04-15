package graph.traversal

import graph._



/**
 * Class for checking graph connectivity.
 *
 * @param graph  the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class BFTConnected[V](graph: UndirectedGraph[V]) extends Connected[V](graph) {
  protected val traversal: FirstTraversal[V] = new BreadthFirstTraversal[V](graph, graph.vertices.head)
}

