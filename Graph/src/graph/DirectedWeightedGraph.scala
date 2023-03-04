package graph

import scala.collection.immutable

/**
 * A trait representing a directed weighted graph.
 *
 * @tparam V  the type of vertices in the graph
 * @tparam W  the type of weights in the graph
 * @tparam WE the kind of edges in the graph
 */
trait DirectedWeightedGraph[V, W, WE[_, _]] extends DirectedGraph[V, ({type E[a] = WE[a, W]})#E]
  with WeightedGraph[V, W, WE] {
  /**
   * Returns a set of the predecessors of a given vertex, along with the weights of the edges leading to them.
   *
   * @param vertex the vertex whose predecessors and weights to return
   * @return a set of tuples where the first element is a predecessor of the vertex and the second element is the weight of the edge leading to it
   */
  def predecessorsAndWeights(vertex: V): immutable.Set[(V, W)]
}
