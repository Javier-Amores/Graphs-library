package graphs

trait WeightedGraph[V,W] extends Graph[V]{
  /**
   * @return the set of edges and their weights.
   */
  def weightedEdges: Set[WeightedEdge[V,W]]

  /**
   * add weight to edge (source, destination)
   * @param source source node
   * @param destination destination node
   * @param weight weight of node (source, destination)
   */
  def addWeight(source:V, destination:V,weight: W):Unit

  /**
   * add weight to edge
   * @param edge edge in the graph.
   * @param weight desired weight to be added.
   */
  def addWeight(edge: Edge[V],weight: W):Unit

  /**
   * @param edge An edge in the graph.
   * @return the weight associated with the edge.
   */
  def getWeight(edge: Edge[V]): W



}
