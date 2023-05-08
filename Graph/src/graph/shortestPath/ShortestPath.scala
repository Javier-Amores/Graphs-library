package graph.shortestPath

trait ShortestPath[V, W, +E[_]] {

  def distTo(v: V): Option[W]

  def hasPathTo(v: V): Boolean

  def pathTo[Edge[X] >: E[X]](v: V): Iterable[Edge[V]]

}
