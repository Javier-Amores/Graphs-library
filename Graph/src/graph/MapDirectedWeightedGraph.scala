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
 * @tparam W the type of weights on the edges in the graph
 */
class MapDirectedWeightedGraph[V, W] extends DirectedWeightedGraph[V, W] {

  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Boolean = if (!containsVertex(vertex)) {
    succsAndWeights(vertex) = mutable.Set[Pair[V, W]]()
    true
  }
  else {
    false
  }

  override def containsVertex(vertex: V): Boolean = succsAndWeights.get(vertex) match {
    case None => false
    case Some(_) => true
  }

  override def deleteVertex(vertex: V): Boolean = if (containsVertex(vertex)) {
    succsAndWeights -= vertex
    for ((_, destinationAndWeightSet) <- succsAndWeights) {
      destinationAndWeightSet filterInPlace (pair => pair.vertex != vertex)
    }
    true

  }
  else {
    false
  }

  override def order: Int = succsAndWeights.size

  override def addEdge(source: V, destination: V, weight: W): Boolean = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs.")
    }
    if (containsEdge(source, destination)) {
      false
    } else {
      succsAndWeights.get(source) match {
        case Some(set) if containsVertex(destination) => set += Pair(destination, weight)
          true
        case _ => false
      }
    }
  }

  override def addEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean = {
    val DirectedWeightedEdge(source, destination, weight) = directedWeightedEdge
    addEdge(source, destination, weight)
  }

  override def containsEdge(source: V, destination: V): Boolean = {
    succsAndWeights.get(source) match {
      case None => false
      case Some(set) => set.map(pair => pair.vertex).contains(destination)
    }
  }

  override def containsEdge(source: V, destination: V, weight: W): Boolean = {
    succsAndWeights.get(source) match {
      case None => false
      case Some(set) => set.contains(Pair(destination, weight))
    }
  }

  override def containsEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean = {
    succsAndWeights.get(directedWeightedEdge.source) match {
      case None => false
      case Some(set) => set.contains(Pair(directedWeightedEdge.destination, directedWeightedEdge.weight))
    }
  }

  override def deleteEdge(source: V, destination: V): Boolean = {
    if (containsEdge(source, destination)) {
      succsAndWeights.get(source) match {
        case Some(set) =>
          set filterInPlace (pair => pair.vertex != destination)
          true
      }
    } else {
      false
    }
  }

  override def deleteEdge(source: V, destination: V, weight: W): Boolean = {
    if (containsEdge(source, destination, weight)) {
      succsAndWeights.get(source) match {
        case Some(set) =>
          set -= Pair(destination, weight)
          true
      }
    } else {
      false
    }
  }

  override def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[V, W]): Boolean = {
    val DirectedWeightedEdge(source, destination, weight) = directedWeightedEdge
    deleteEdge(source, destination, weight)
  }

  override def edges[Edge[X] >: DirectedWeightedEdge[X, W]]: immutable.Set[Edge[V]] = {
    var edgeSet = immutable.Set[Edge[V]]()
    for ((source, destinationSet) <- succsAndWeights) {
      destinationSet.foreach(pair => edgeSet += DirectedWeightedEdge(source, pair.vertex, pair.weight))
    }
    edgeSet
  }

  override def vertices: immutable.Set[V] = {
    immutable.Set.empty ++ succsAndWeights.keys
  }

  override def weightOfEdge(source: V, destination: V): Option[W] = {
    succsAndWeights.get(source) match {
      case Some(set) if containsEdge(source, destination) => Some(set.collect { case Pair(vertex, weight) if vertex == destination => weight }.head)
      case _ => None
    }
  }

  override def size: Int = {
    var sum: Int = 0
    for ((_, destinationSet) <- succsAndWeights) {
      sum += destinationSet.size
    }
    sum
  }

  override def successors(source: V): immutable.Set[V] = {
    succsAndWeights.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => pair.vertex)
    }
  }

  override def predecessors(destination: V): immutable.Set[V] = if (containsVertex(destination)) {
    var predecessorSet = immutable.Set[V]()
    for ((predecessor, destinationSet) <- succsAndWeights) {
      if (destinationSet.map(pair => pair.vertex).contains(destination)) {
        predecessorSet += predecessor
      }
    }
    predecessorSet
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }

  override def incidentsFrom[Edge[X] >: DirectedWeightedEdge[X, W]](source: V): immutable.Set[Edge[V]] = {
    succsAndWeights.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(successorSet) => var edgeSet = immutable.Set[Edge[V]]()
        successorSet.foreach(successorPair => edgeSet += DirectedWeightedEdge(source, successorPair.vertex, successorPair.weight))
        edgeSet
    }
  }

  override def incidentsTo[Edge[X] >: DirectedWeightedEdge[X, W]](destination: V): immutable.Set[Edge[V]] = if (containsVertex(destination)) {
    var edgeSet = immutable.Set[Edge[V]]()
    for ((predecessor, destinationSet) <- succsAndWeights) {
      destinationSet.foreach { case Pair(otherDestination, weight) if otherDestination == destination => edgeSet += DirectedWeightedEdge(predecessor, destination, weight); case _ => }
    }
    edgeSet
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }

  override def outdegree(source: V): Int = {
    succsAndWeights.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) => set.size
    }
  }

  override def indegree(destination: V): Int = if (containsVertex(destination)) {
    var sum: Int = 0
    for ((_, destinationSet) <- succsAndWeights) {
      if (destinationSet.map(pair => pair.vertex).contains(destination)) {
        sum += 1
      }
    }
    sum
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }
}
