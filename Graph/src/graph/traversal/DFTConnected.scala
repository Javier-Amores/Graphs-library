package graph.traversal

import graph.UndirectedGraph

case class DFTConnected[V](graph: UndirectedGraph[V]) extends Connected[V](graph) {
  protected val traversal: FirstTraversal[V] = new DepthFirstTraversal[V](graph, graph.vertices.head)
}
