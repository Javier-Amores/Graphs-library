package graph.traversal

import scala.collection.mutable


trait Traversal[V] {
  def isReachable(vertex: V): Boolean

  def pathTo(vertex: V): Option[List[V]]

  def getSpanningTree(): mutable.Map[V, mutable.Set[V]]
}
