package graph.traversal


trait Traversal[V] {
  def isReachable(vertex: V): Boolean

  def pathTo(vertex: V): Option[List[V]]
}
