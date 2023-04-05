package graph.traversal

import graph._

case class Connected[V](graph: UndirectedGraph[V], method:String = "BFT"){
  private val startVertex = graph.vertices.head
  private val traversal = {if (method == "BFT") {new BreadthFirstTraversal[V](graph, startVertex)}
  else if (method == "DFT") {new DepthFirstTraversal[V](graph, startVertex)}
  else {throw GraphException("Traversal algorithm not found. Valid inputs: \"DFT\" (Depth first traversal), \"BFT\" (Breadth first traversal).")}}
  def isConnected: Boolean = graph.vertices.forall(traversal.isReachable)

}
