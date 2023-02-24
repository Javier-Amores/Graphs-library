package graph

import scala.collection.{immutable, mutable}

object MapDirectedWeightedGraph {
  def apply[V, W](): MapDirectedWeightedGraph[V, W] = new MapDirectedWeightedGraph()
}

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
    val predecessorAndWeightSet = mutable.Set[(V,W)]()
    for ((predecessor, destinationAndWeightSet) <- succsAndWeights) {
      if (destinationAndWeightSet.map(pair => pair.vertex).contains(vertex)) {
        predecessorAndWeightSet += (predecessor,destinationAndWeightSet.collect{case pair: Pair[V,W] if pair.vertex == vertex => pair.weight}.head)
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

  override def addEdge(source: V, destination: V): DirectedWeightedEdge[V, W] = ???

  override def addEdge(source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] = ???

  override def addEdge(edge: DirectedWeightedEdge[V, W]): Unit = ???

  override def deleteEdge(edge: DirectedWeightedEdge[V, W]): Unit = ???

  override def containsEdge(edge: DirectedWeightedEdge[V, W]): Boolean = ???

  override def edges: Set[DirectedWeightedEdge[V, W]] = ???

  override def size: Int = ???
}
