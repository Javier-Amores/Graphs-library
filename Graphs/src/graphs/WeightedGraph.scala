package graphs

trait WeightedGraph[V,W] extends Graph[V]{
  /**
   * @return
   */
  def weightedEdges: Set[WeightedEdge[V,W]]

  /**
   *
   * @param source
   * @param destination
   * @param weight
   */
  def addWeight(source:V, destination:V,weight: W):Unit

  /**
   * @param edge
   * @param weight
   */
  def addWeight(edge: Edge[V],weight: W):Unit

  def getWeight(edge: Edge[V]): W



}
