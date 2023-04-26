package graph.traversal

import graph.WeightedEdge

import scala.collection.mutable

trait MinimumSpanningTree[V,W] {

  def getMstEdges: mutable.Set[WeightedEdge[V,W]]

  def totalWeight:W

}
