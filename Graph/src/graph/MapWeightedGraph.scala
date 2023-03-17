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
 * @tparam W the type of weights on the edges in the graph
 */
class MapWeightedGraph[V, W] extends UndirectedWeightedGraph[V, W] {

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

  override def deleteVertex(vertex: V): Boolean = {
    succsAndWeights.remove(vertex) match {
      case None => false
      case Some(successorsAndWeightSet) => successorsAndWeightSet.foreach(successorAndWeight => succsAndWeights.get(successorAndWeight.vertex) match {
        case Some(set) => set.remove(Pair(vertex, successorAndWeight.weight))

      })
        true
    }
  }

  override def order: Int = succsAndWeights.size

  override def addEdge(vertex1: V, vertex2: V, weight: W): Boolean = {
    if (vertex1 == vertex2) {
      return false
    }
    if (containsEdge(vertex1, vertex2)) {
      return false
    }
    (succsAndWeights.get(vertex1), succsAndWeights.get(vertex2)) match {
      case (None, _) => false
      case (_, None) => false
      case (Some(set1), Some(set2)) => set1 += Pair(vertex2, weight)
        set2 += Pair(vertex1, weight)
        true
    }
  }

  override def addEdge(weightedEdge: WeightedEdge[V, W]): Boolean = {
    if (containsEdge(weightedEdge.vertex1, weightedEdge.vertex2)) {
      return false
    }
    if (weightedEdge.vertex1 == weightedEdge.vertex2) {
      return false
    }
    (succsAndWeights.get(weightedEdge.vertex1), succsAndWeights.get(weightedEdge.vertex2)) match {
      case (None, _) => false
      case (_, None) => false
      case (Some(set1), Some(set2)) => set1 += Pair(weightedEdge.vertex2, weightedEdge.weight)
        set2 += Pair(weightedEdge.vertex1, weightedEdge.weight)
        true
    }
  }

  override def containsEdge(vertex1: V, vertex2: V): Boolean = {
    succsAndWeights.get(vertex1) match {
      case None => false
      case Some(set) => set.map(pair => pair.vertex).contains(vertex2)
    }
  }

  override def containsEdge(vertex1: V, vertex2: V, weight: W): Boolean = {
    succsAndWeights.get(vertex1) match {
      case None => false
      case Some(set) => set.contains(Pair(vertex2, weight))
    }
  }

  override def containsEdge(weightedEdge: WeightedEdge[V, W]): Boolean = {
    succsAndWeights.get(weightedEdge.vertex1) match {
      case None => false
      case Some(set) => set.contains(Pair(weightedEdge.vertex2, weightedEdge.weight))
    }
  }

  override def deleteEdge(vertex1: V, vertex2: V): Boolean = {
    if (!containsEdge(vertex1, vertex2)) {
      return false
    }
    (succsAndWeights.get(vertex1), succsAndWeights.get(vertex2)) match {
      case (Some(set1), Some(set2)) => set1.filterInPlace(pair => pair.vertex != vertex2)
        set2.filterInPlace(pair => pair.vertex != vertex1)
        true
      case _ => false
    }
  }

  override def deleteEdge(vertex1: V, vertex2: V, weight: W): Boolean = {
    if (!containsEdge(vertex1, vertex2, weight)) {
      return false
    }
    (succsAndWeights.get(vertex1), succsAndWeights.get(vertex2)) match {
      case (Some(set1), Some(set2)) => set1 -= Pair(vertex2, weight)
        set2 -= Pair(vertex1, weight)
        true
      case _ => false
    }
  }

  override def deleteEdge(weightedEdge: WeightedEdge[V, W]): Boolean = {
    if (!containsEdge(weightedEdge)) {
      return false
    }
    (succsAndWeights.get(weightedEdge.vertex1), succsAndWeights.get(weightedEdge.vertex2)) match {
      case (Some(set1), Some(set2)) => set1 -= Pair(weightedEdge.vertex2, weightedEdge.weight)
        set2 -= Pair(weightedEdge.vertex1, weightedEdge.weight)
        true
      case _ => false
    }
  }

  override def edges[Edge[X] >: WeightedEdge[X, W]]: immutable.Set[Edge[V]] = {
    var edgeSet = immutable.Set[Edge[V]]()
    for ((vertex, adjacentSet) <- succsAndWeights) {
      adjacentSet.foreach(pair => edgeSet += WeightedEdge(vertex, pair.vertex, pair.weight))
    }
    edgeSet
  }

  override def vertices: immutable.Set[V] = {
    immutable.Set.empty ++ succsAndWeights.keys
  }

  override def weightOfEdge(vertex1: V, vertex2: V): Option[W] = {
    succsAndWeights.get(vertex1) match {
      case Some(set) if containsEdge(vertex1, vertex2) => Some(set.collect { case Pair(vertex, weight) if vertex == vertex2 => weight }.head)
      case _ => None
    }
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

  override def adjacents(vertex: V): immutable.Set[V] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => pair.vertex)
    }
  }
  /*
    override def incidents[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = {
      succsAndWeights.get(vertex) match {
        case None => throw GraphException(s"Vertex $vertex not found.")
        case Some(set) => immutable.Set.empty ++ set.map(pair => WeightedEdge(vertex, pair.vertex, pair.weight).asInstanceOf[Edge[V]])
      }
    }*/

  override def degree(vertex: V): Int = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }

  override def incidentsFrom[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(successorSet) => var edgeSet = immutable.Set[Edge[V]]()
        successorSet.foreach(successorPair => edgeSet += WeightedEdge(vertex, successorPair.vertex, successorPair.weight))
        edgeSet
    }
  }

  override def incidentsTo[Edge[X] >: WeightedEdge[X, W]](vertex: V): immutable.Set[Edge[V]] = {
    succsAndWeights.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(pair => WeightedEdge(pair.vertex, vertex, pair.weight).asInstanceOf[Edge[V]])
    }
  }
}
