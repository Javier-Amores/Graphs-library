package graph.MST

import graph.WeightedEdge

import scala.collection.mutable


trait PrimMST[V, W] extends MinimumSpanningTree[V, W] {

  /**
   * Executes the Prim's algorithm to find the minimum spanning tree of the input graph.
   *
   * @return the total weight of the minimum spanning tree
   */
  protected def main(): W


}
