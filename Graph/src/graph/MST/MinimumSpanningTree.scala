package graph.MST

import graph.WeightedEdge

import scala.collection.mutable

/**
 * A trait representing a minimum spanning tree algorithm for a graph.
 *
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights associated with the edges in the graph
 */
trait MinimumSpanningTree[V, W] {

  /**
   * Returns the set of edges that form the minimum spanning tree.
   *
   * @return a mutable set of weighted edges that form the minimum spanning tree
   */
  def getMstEdges: Set[WeightedEdge[V, W]]

  /**
   * Returns the weight of the minimum spanning tree.
   *
   * @return the weight of the minimum spanning tree
   */
  def totalWeight: W

}
