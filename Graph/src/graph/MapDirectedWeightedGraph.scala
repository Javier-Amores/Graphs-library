package graph

import scala.collection.{immutable, mutable}

object MapDirectedWeightedGraph {
  def apply[V, W](): MapDirectedWeightedGraph[V, W] = new MapDirectedWeightedGraph()
}

/**
 * Represents a directed weighted graph, where each vertex is represented by a key in a mutable map, and
 * the value associated with each key is a mutable set of Pairs whose first component represents the successors of that
 * vertex and the second one represents the weight of the edge.
 *
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
      destinationAndWeightSet filterInPlace (pair => pair.vertex != vertex)
    }

  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }


  override def containsVertex(vertex: V): Boolean = succsAndWeights.get(vertex) match {
    case None => false
    case Some(_) => true
  }

  override def vertices: immutable.Set[V] = {
    val immutableVertices = immutable.Set.empty ++ succsAndWeights.keys
    immutableVertices
  }

  override def order: Int = succsAndWeights.size

  override def successors(vertex: V): immutable.Set[V] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => pair.vertex)
    }
  }

  override def predecessors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    var predecessorSet = immutable.Set[V]()
    for ((predecessor, destinationSet) <- succsAndWeights) {
      if (destinationSet.map(pair => pair.vertex).contains(vertex)) {
        predecessorSet += predecessor
      }
    }
    predecessorSet
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => (pair.vertex, pair.weight))
    }
  }


  override def predecessorsAndWeights(vertex: V): immutable.Set[(V, W)] = if (containsVertex(vertex)) {
    var predecessorAndWeightSet = immutable.Set[(V, W)]()
    for ((predecessor, destinationAndWeightSet) <- succsAndWeights) {
      if (destinationAndWeightSet.map(pair => pair.vertex).contains(vertex)) {
        val predecessorAndWeight: (V, W) = (predecessor, destinationAndWeightSet.collect { case pair: Pair[V, W] if pair.vertex == vertex => pair.weight }.head)
        predecessorAndWeightSet += predecessorAndWeight
      }
    }
    predecessorAndWeightSet
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def degree(vertex: V): Int = indegree(vertex) + outdegree(vertex)

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

  override def outdegree(vertex: V): Int = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }

  override def addEdge(source: V, destination: V): DirectedWeightedEdge[V, W] = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val edge = DirectedWeightedEdge(source, destination, null.asInstanceOf[W])
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(source, destination)} is already in the graph.")
    }
    succsAndWeights.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) if containsVertex(destination) => set += Pair(destination, null.asInstanceOf[W])
        edge
      case _ => throw GraphException(s"Vertex $destination not found.")
    }
  }

  override def addEdge(source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val edge = DirectedWeightedEdge(source, destination, weight)
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(source, destination)} is already in the graph.")
    }
    succsAndWeights.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) if containsVertex(destination) => set += Pair(destination, weight)
        edge
      case _ => throw GraphException(s"Vertex $destination not found.")
    }
  }


  override def addEdge(edge: DirectedWeightedEdge[V, W]): Unit = {
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(edge.source, edge.destination)} is already in the graph.")
    }
    if (edge.source == edge.destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }

    succsAndWeights.get(edge.source) match {
      case None => throw GraphException(s"Vertex ${edge.source} not found.")
      case Some(set) if containsVertex(edge.destination) => set += Pair(edge.destination, edge.weight)
      case _ => throw GraphException(s"Vertex ${edge.destination} not found.")
    }
  }

  override def deleteEdge(edge: DirectedWeightedEdge[V, W]): Unit = {
    if (!containsEdge(edge)) {
      throw GraphException(s"Edge $edge not found.")
    }

    succsAndWeights.get(edge.source) match {
      case None => throw GraphException(s"Vertex ${edge.source} not found.")
      case Some(set) if containsVertex(edge.destination) => set -= Pair(edge.destination, edge.weight)
      case _ => throw GraphException(s"Vertex ${edge.destination} not found.")
    }
  }

  override def containsEdge(edge: DirectedWeightedEdge[V, W]): Boolean = {
    succsAndWeights.get(edge.source) match {
      case None => false
      case Some(set) => set.contains(Pair(edge.destination, edge.weight))
    }
  }

  /**
   * Checks if the graph contains an edge regardless of the weight
   *
   * @param edge the edge to check
   * @return true if the graph contains the edge, false otherwise
   */
  private def containsEdgeAnyWeight(edge: DirectedEdge[V]): Boolean = {
    succsAndWeights.get(edge.source) match {
      case None => false
      case Some(set) => set.map(pair => pair.vertex).contains(edge.destination)
    }
  }

  override def edges: Set[DirectedWeightedEdge[V, W]] = {
    var edgeSet = immutable.Set[DirectedWeightedEdge[V, W]]()
    for ((source, destinationSet) <- succsAndWeights) {
      destinationSet.foreach(pair => edgeSet += DirectedWeightedEdge(source, pair.vertex, pair.weight))
    }
    edgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    for ((_, destinationSet) <- succsAndWeights) {
      sum += destinationSet.size
    }
    sum
  }
}
