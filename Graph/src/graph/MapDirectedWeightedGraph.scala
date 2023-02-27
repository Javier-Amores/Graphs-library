package graph

import scala.collection.{immutable, mutable}

object MapDirectedWeightedGraph {
  def apply[V, W](): MapDirectedWeightedGraph[V, W] = new MapDirectedWeightedGraph()
}

/**
 * Represents a directed weighted graph, where each vertex is represented by a key in a mutable map, and
 * the value associated with each key is a mutable set of Pairs whose first component represents the successors of that
 * vertex and the second one represents the weight of the edge.
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights in the graph
 */
class MapDirectedWeightedGraph[V, W] extends DirectedWeightedGraph[V, W, DirectedWeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Unit = if (!containsVertex(vertex)) {
    succsAndWeights(vertex) = mutable.Set[Pair[V, W]]()
  }
  else {
    throw GraphException(s"Vertex $vertex is already in the graph.")
  }

  override def deleteVertex(vertex: V): Unit = if (containsVertex(vertex)) {
    succsAndWeights -= vertex
    for ((_, destinationAndWeightSet) <- succsAndWeights) {
      destinationAndWeightSet filterInPlace (pair => pair.vertex!=vertex)
    }

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

  override def predecessors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    val predecessorSet = mutable.Set[V]()
    for ((predecessor, destinationSet) <- succsAndWeights) {
      if (destinationSet.map(pair => pair.vertex).contains(vertex)) {
        predecessorSet += predecessor
      }
    }
    val immutablePredecessorSet = immutable.Set.empty ++ predecessorSet
    immutablePredecessorSet
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


  override def predecessorsAndWeights(vertex: V): immutable.Set[(V, W)] = if (containsVertex(vertex)) {
    val predecessorAndWeightSet = mutable.Set[(V, W)]()
    for ((predecessor, destinationAndWeightSet) <- succsAndWeights) {
      if (destinationAndWeightSet.map(pair => pair.vertex).contains(vertex)) {
        val predecessorAndWeight: (V,W) = (predecessor,destinationAndWeightSet.collect {case pair: Pair[V, W] if pair.vertex == vertex => pair.weight}.head)
        predecessorAndWeightSet += predecessorAndWeight
      }
    }
    val immutablePredecessorSet: immutable.Set[(V, W)] = immutable.Set.empty ++ predecessorAndWeightSet
    immutablePredecessorSet
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def degree(vertex: V): Int = indegree(vertex)+outdegree(vertex)

  override def indegree(vertex: V): Int = if (containsVertex(vertex)) {
    var sum: Int = 0
    for ((_, destinationSet) <- succsAndWeights) {
      if (destinationSet.map(pair => pair.vertex).contains(vertex)) {
        sum += 1
      }
    }
    sum
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def outdegree(vertex: V): Int = if (containsVertex(vertex)) {
    succsAndWeights(vertex).size
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def addEdge(source: V, destination: V): DirectedWeightedEdge[V, W] = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    else {
      val edge = DirectedWeightedEdge(source, destination, null.asInstanceOf[W])
      if (containsEdgeAnyWeight(edge)) {
        throw GraphException(s"${Edge(edge.source, edge.destination)} is already in the graph.")
      }
      else if (!containsVertex(source)) {
        throw GraphException(s"Vertex $source not found.")
      }
      else if (!containsVertex(destination)) {
        throw GraphException(s"Vertex $destination not found.")
      }
      else {
        succsAndWeights(source) += Pair(destination, null.asInstanceOf[W])
        edge
      }
    }

  }

  override def addEdge(source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    else {
      val edge = DirectedWeightedEdge(source, destination, weight)
      if (containsEdgeAnyWeight(edge)) {
        throw GraphException(s"${Edge(edge.source, edge.destination)} is already in the graph.")
      }
      else if (!containsVertex(source)) {
        throw GraphException(s"Vertex $source not found.")
      }
      else if (!containsVertex(destination)) {
        throw GraphException(s"Vertex $destination not found.")
      }
      else {
        succsAndWeights(source) += Pair(destination, weight)
        edge
      }
    }

  }

  override def addEdge(edge: DirectedWeightedEdge[V, W]): Unit = if (containsEdgeAnyWeight(edge)) {
    throw GraphException(s"${DirectedEdge(edge.source,edge.destination)} is already in the graph.")
  }
  else if (!containsVertex(edge.source)) {
    throw GraphException(s"Vertex ${edge.source} not found.")
  }
  else if (edge.source == edge.destination) {
    throw GraphException("Self-loops are not allowed in simple graphs")
  }
  else if (!containsVertex(edge.source)) {
    throw GraphException(s"Vertex ${edge.source} not found.")
  }
  else {
    succsAndWeights(edge.source) += Pair(edge.destination, edge.weight)
  }

  override def deleteEdge(edge: DirectedWeightedEdge[V, W]): Unit = if (!containsEdge(edge)) {
    throw GraphException(s"Edge $edge not found.")
  }
  else if (!containsVertex(edge.source)) {
    throw GraphException(s"Vertex ${edge.source} not found.")
  }
  else if (!containsVertex(edge.destination)) {
    throw GraphException(s"Vertex ${edge.destination} not found.")
  }
  else {
    succsAndWeights(edge.source) -= Pair(edge.destination, edge.weight)
  }

  override def containsEdge(edge: DirectedWeightedEdge[V, W]): Boolean = containsVertex(edge.source) && (succsAndWeights(edge.source) contains Pair(edge.destination,edge.weight))

  /**
   * Checks if the graph contains an edge regardless of the weight
   * @param edge the edge to check
   * @return true if the graph contains the edge, false otherwise
   */
  private def containsEdgeAnyWeight(edge: DirectedEdge[V]): Boolean = containsVertex(edge.source) && (succsAndWeights(edge.source).map(pair => pair.vertex) contains edge.destination)

  override def edges: Set[DirectedWeightedEdge[V, W]] = {
    val edgeSet = mutable.Set[DirectedWeightedEdge[V,W]]()
    for ((source, _) <- succsAndWeights) {
      succsAndWeights(source).foreach(pair => edgeSet += DirectedWeightedEdge(source, pair.vertex,pair.weight))
    }
    val finalEdgeSet = Set.empty ++ edgeSet
    finalEdgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    for ((source, _) <- succsAndWeights) {
      sum += succsAndWeights(source).size
    }
    sum
  }
}
