package graph.traversal

import graph.UndirectedGraph


/**
 *
 * class for computing connected components of an undirected graph using Depth-First Traversal.
 *
 * @param graph the graph to compute the connected components of
 * @tparam V the type of the vertices in the graph
 */
case class DFTConnectedComponent[V](graph: UndirectedGraph[V]) extends ConnectedComponent[V](graph) {

  protected def traversal(starVertex: V): FirstTraversal[V] = new DepthFirstTraversal[V](graph, starVertex)


}

