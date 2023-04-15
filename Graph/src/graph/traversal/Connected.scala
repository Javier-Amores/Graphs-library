package graph.traversal

import graph.UndirectedGraph

abstract class Connected[V](graph: UndirectedGraph[V]) {

  protected val traversal:FirstTraversal[V]

  def isConnected: Boolean = graph.vertices.forall(traversal.isReachable)

}
