package graph

import scala.collection.immutable

trait DirectedWeightedGraph[V, W, WE[_, _]] extends DirectedGraph[V, ({type E[a] = WE[a, W]})#E]
  with WeightedGraph[V, W, WE] {
  def predecessorsAndWeights(vertex: V): immutable.Set[(V, W)]
}
