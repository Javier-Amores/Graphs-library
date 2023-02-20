package graph

import scala.collection.immutable

trait WeightedGraph[V, W, WE[_, _]] extends Graph[V, ({type E[a] = WE[a, W]})#E] {
  def addEdge(vertex1: V, vertex2: V, weight: W): WE[V, W]

  def successorsAndWeights(vertex: V): immutable.Set[(V, W)]
}
