package graph.traversal

import graph.UndirectedGraph

import scala.collection.mutable

case class BFTConnectedComponent[V](graph: UndirectedGraph[V]) extends ConnectedComponent[V](graph){
  protected def traversal(starVertex:V): FirstTraversal[V] = new BreadthFirstTraversal[V](graph,starVertex)
}
