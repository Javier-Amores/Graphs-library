package graphs

trait WeightedGraph[V,W] extends Graph[V]{
  def weightedEdges: Set[WeightedEdge[V,W]]
  def addWeight(source:V, destination:V,weight: W):Unit
  def addWeight(edge: Edge[V],weight: W):Unit



}
