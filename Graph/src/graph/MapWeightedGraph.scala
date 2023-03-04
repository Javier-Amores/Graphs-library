package graph

import scala.collection.{immutable, mutable}

object MapWeightedGraph {
  def apply[V, W](): MapWeightedGraph[V, W] = new MapWeightedGraph()
}

/**
 * Represents a weighted graph, where each vertex is represented by a key in a mutable map, and
 * the value associated with each key is a mutable set of Pairs whose first component represents the successors of that
 * vertex and the second one represents the weight of the edge.
 *
 * @tparam V the type of vertices in the graph
 * @tparam W the type of the weights associated with the edges
 */
class MapWeightedGraph[V, W] extends WeightedGraph[V, W, WeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Unit = if (!containsVertex(vertex)) {
    succsAndWeights(vertex) = mutable.Set[Pair[V, W]]()
  }
  else {
    throw GraphException(s"Vertex $vertex is already in the graph.")
  }

  override def deleteVertex(vertex: V): Unit = {
    succsAndWeights.remove(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(successorsAndWeightSet) => successorsAndWeightSet.foreach(successorAndWeight => succsAndWeights.get(successorAndWeight.vertex) match {
        case Some(set) => set.remove(Pair(vertex, successorAndWeight.weight))
      })
    }
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


  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => (pair.vertex, pair.weight))
    }
  }

  override def degree(vertex: V): Int = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }

  override def addEdge(vertex1: V, vertex2: V): WeightedEdge[V, W] = {
    if (vertex1 == vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val edge = WeightedEdge(vertex1, vertex2, null.asInstanceOf[W])
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(edge.vertex1, edge.vertex2)} is already in the graph.")
    }
    (succsAndWeights.get(vertex1), succsAndWeights.get(vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex $vertex1 not found.")
      case (_, None) => throw GraphException(s"Vertex $vertex2 not found.")
      case (Some(set1), Some(set2)) => set1 += Pair(vertex2, null.asInstanceOf[W])
        set2 += Pair(vertex1, null.asInstanceOf[W])
        edge
    }
  }

  override def addEdge(vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] = {
    if (vertex1 == vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val edge = WeightedEdge(vertex1, vertex2, weight)
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(vertex1, vertex2)} is already in the graph.")
    }
    (succsAndWeights.get(vertex1), succsAndWeights.get(vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex $vertex1 not found.")
      case (_, None) => throw GraphException(s"Vertex $vertex2 not found.")
      case (Some(set1), Some(set2)) => set1 += Pair(vertex2, weight)
        set2 += Pair(vertex1, weight)
        edge
    }
  }

  override def addEdge(edge: WeightedEdge[V, W]): Unit = {
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(edge.vertex1, edge.vertex2)} is already in the graph.")
    }
    if (edge.vertex1 == edge.vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    (succsAndWeights.get(edge.vertex1), succsAndWeights.get(edge.vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex ${edge.vertex1} not found.")
      case (_, None) => throw GraphException(s"Vertex ${edge.vertex2} not found.")
      case (Some(set1), Some(set2)) => set1 += Pair(edge.vertex2, edge.weight)
        set2 += Pair(edge.vertex1, edge.weight)
    }
  }

  override def deleteEdge(edge: WeightedEdge[V, W]): Unit = {
    if (!containsEdge(edge)) {
      throw GraphException(s"Edge $edge not found.")
    }
    (succsAndWeights.get(edge.vertex1), succsAndWeights.get(edge.vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex ${edge.vertex1} not found.")
      case (_, None) => throw GraphException(s"Vertex ${edge.vertex2} not found.")
      case (Some(set1), Some(set2)) => set1 -= Pair(edge.vertex2, edge.weight)
        set2 -= Pair(edge.vertex1, edge.weight)
    }
  }

  override def containsEdge(edge: WeightedEdge[V, W]): Boolean = {
    succsAndWeights.get(edge.vertex1) match {
      case None => false
      case Some(set) => set.contains(Pair(edge.vertex2, edge.weight))
    }
  }


  /**
   * Checks if the graph contains an edge regardless of the weight
   *
   * @param edge the edge to check
   * @return true if the graph contains the edge, false otherwise
   */
  private def containsEdgeAnyWeight(edge: Edge[V]): Boolean = {
    succsAndWeights.get(edge.vertex1) match {
      case None => false
      case Some(set) => set.map(pair => pair.vertex).contains(edge.vertex2)
    }
  }


  override def edges: Set[WeightedEdge[V, W]] = {
    var edgeSet = immutable.Set[WeightedEdge[V, W]]()
    val visited = mutable.Set[V]()
    for ((vertex1, successorSet) <- succsAndWeights) {
      successorSet.filter(pair => !visited.contains(pair.vertex)).foreach(pair => edgeSet += WeightedEdge(vertex1, pair.vertex, pair.weight))
      visited += vertex1
    }
    edgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    val visited = mutable.Set[V]()
    for ((vertex, successorSet) <- succsAndWeights) {
      sum += successorSet.count(pair => !visited.contains(pair.vertex))
      visited += vertex
    }
    sum
  }
}
