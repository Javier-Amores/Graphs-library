package graph.traversal

import graph._


case class ConnectedComponent[V](graph: UndirectedGraph[V], method:String = "BFT") {

  private def traversal(startVertex:V):Traversal[V] = {
    if (method == "BFT") {
      new BreadthFirstTraversal[V](graph, startVertex)
    }
    else if (method == "DFT") {
      new DepthFirstTraversal[V](graph, startVertex)
    }
    else {
      throw GraphException("Traversal algorithm not found. Valid inputs: \"DFT\" (Depth first traversal), \"BFT\" (Breadth first traversal).")
    }
  }

  def get(vertex: V) : Set[V] = {
    val connectedComponent = graph.vertices.collect { case currentVertex if traversal(vertex).isReachable(currentVertex) => currentVertex }
    connectedComponent

  }

  def get(): Set[Set[V]] = {
    var vertices: Set[V] = graph.vertices
    var connectedComponents = Set[Set[V]]()
    var currentComponent = Set[V]()
    var currentVertex = null.asInstanceOf[V]
    var currentTraversal = null.asInstanceOf[Traversal[V]]
    while (vertices.nonEmpty) {
      currentVertex = vertices.head
      currentTraversal = traversal(currentVertex)
      currentComponent = {vertices.collect {case vertex if currentTraversal.isReachable(vertex) => vertex}}
      connectedComponents += currentComponent
      vertices = vertices.diff(currentComponent)
    }
    connectedComponents
  }

  def getNumber: Int = get().size


}
