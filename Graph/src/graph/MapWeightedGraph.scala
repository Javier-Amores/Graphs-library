package graph

import scala.collection.{immutable, mutable}

object MapWeightedGraph {
  def apply[V, W](): MapWeightedGraph[V, W] = new MapWeightedGraph()
}

class MapWeightedGraph[V, W] extends WeightedGraph[V, W, WeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Unit = if (!containsVertex(vertex)) {
    succsAndWeights(vertex) = mutable.Set[Pair[V,W]]()
  }
  else {
    throw GraphException(s"Vertex $vertex is already in the graph.")
  }

  override def deleteVertex(vertex: V): Unit = if (containsVertex(vertex)) {
    val successorsAndWeights = succsAndWeights.remove(vertex)
    successorsAndWeights.get.foreach(successorAndWeight => succsAndWeights(successorAndWeight.vertex) remove Pair(vertex, successorAndWeight.weight))
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def containsVertex(vertex: V): Boolean = succsAndWeights.contains(vertex)

  override def vertices: immutable.Set[V] = {
    val immutableVertices = immutable.Set.empty ++ succsAndWeights.keySet
    immutableVertices
  }

  override def order: Int = succsAndWeights.size

  override def successors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    val immutableSuccs = immutable.Set.empty ++ succsAndWeights(vertex).map(pair => pair.vertex)
    immutableSuccs
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = if (containsVertex(vertex)) {
    val immutableSuccs = immutable.Set.empty ++ succsAndWeights(vertex).map(pair => (pair.vertex,pair.weight))
    immutableSuccs
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def degree(vertex: V): Int = if (containsVertex(vertex)) {
    succsAndWeights(vertex).size
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def addEdge(vertex1: V, vertex2: V): WeightedEdge[V, W] = ???

  override def addEdge(vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] = ???

  override def addEdge(edge: WeightedEdge[V, W]): Unit = if (containsEdgeAnyWeight(edge)) {
    throw GraphException(s"${Edge(edge.vertex1,edge.vertex2)} is already in the graph.")
  }
  else if (!containsVertex(edge.vertex1)) {
    throw GraphException(s"Vertex ${edge.vertex1} not found.")
  }
  else if (edge.vertex1==edge.vertex2) {
    throw GraphException("Self-loops are not allowed in simple graphs")
  }
  else if (!containsVertex(edge.vertex2)) {
    throw GraphException(s"Vertex ${edge.vertex2} not found.")
  }
  else {
    succsAndWeights(edge.vertex1) += Pair(edge.vertex2,edge.weight)
    succsAndWeights(edge.vertex2) += Pair(edge.vertex1,edge.weight)
  }

  override def deleteEdge(edge: WeightedEdge[V, W]): Unit = if (!containsEdge(edge)) {
    throw GraphException(s"Edge $edge not found.")
  }
  else if (!containsVertex(edge.vertex1)) {
    throw GraphException(s"Vertex ${edge.vertex1} not found.")
  }
  else if (!containsVertex(edge.vertex2)) {
    throw GraphException(s"Vertex ${edge.vertex2} not found.")
  }
  else {
    succsAndWeights(edge.vertex1) -= Pair(edge.vertex2,edge.weight)
    succsAndWeights(edge.vertex2) -= Pair(edge.vertex1,edge.weight)
  }

  override def containsEdge(edge: WeightedEdge[V, W]): Boolean = containsVertex(edge.vertex1) && (succsAndWeights(edge.vertex1) contains Pair(edge.vertex2,edge.weight))

  private def containsEdgeAnyWeight(edge: Edge[V]): Boolean = containsVertex(edge.vertex1) && (succsAndWeights(edge.vertex1).map(pair => pair.vertex) contains edge.vertex2)

  override def edges: Set[WeightedEdge[V, W]] = {
    val edgeSet = mutable.Set[WeightedEdge[V,W]]()
    val visited = mutable.Set[V]()
    for ((vertex1, _) <- succsAndWeights) {
      succsAndWeights(vertex1).filter(pair => !visited.contains(pair.vertex)).foreach(pair => edgeSet += WeightedEdge(vertex1, pair.vertex,pair.weight))
      visited += vertex1
    }
    val finalEdgeSet = Set.empty ++ edgeSet
    finalEdgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    val visited = mutable.Set[V]()
    for ((vertex, _) <- succsAndWeights) {
      sum += succsAndWeights(vertex).count(pair => !visited.contains(pair.vertex))
      visited += vertex
    }
    sum
  }
}
