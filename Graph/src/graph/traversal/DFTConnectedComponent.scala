package graph.traversal

import graph.UndirectedGraph

import scala.collection.{immutable, mutable}

case class DFTConnectedComponent[V](graph: UndirectedGraph[V]) extends ConnectedComponent[V](graph){

  protected def traversal(starVertex:V): FirstTraversal[V] = new DepthFirstTraversal[V](graph,starVertex)


}

