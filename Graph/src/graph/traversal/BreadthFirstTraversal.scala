package graph.traversal

import graph._

import scala.collection.mutable


class BreadthFirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends FirstTraversal[V](graph, startVertex) {
  protected val structure: Structure[IsEdge[V]] = new Queue[IsEdge[V]]
  protected val spanningTree: mutable.Map[V, mutable.Set[V]] = traverse()
}