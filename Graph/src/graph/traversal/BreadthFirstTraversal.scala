package graph.traversal

import graph._

import scala.collection.mutable


/**
 * Implements a breadth-first traversal of a graph, starting from a specified vertex.
 *
 * @param graph       the graph to traverse
 * @param startVertex the vertex to start the traversal from
 * @tparam V the type of the vertices in the graph
 */
class BreadthFirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends FirstTraversal[V](graph, startVertex) {
  protected val container: Container[IsEdge[V]] = new Queue[IsEdge[V]]
  protected val spanningTree: mutable.Map[V, V] = traverse()
}