package graph.shortestPath

import graph.{DirectedWeightedEdge, Edge, IsWeighted, IsWeightedEdge}

trait DijkstraShortestPath[V, W,+E[_]] {

  def distTo(v:V): Option[W]

  def hasPathTo(v:V):Boolean

  def pathTo[Edge[X] >: E[X]](v:V):Iterable[Edge[V]]

}
