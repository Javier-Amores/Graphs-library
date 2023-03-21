package graph

import scala.collection.{immutable, mutable}

object MapDirectedGraph {
  def apply[V](): MapDirectedGraph[V] = new MapDirectedGraph()
}

/**
 * Represents a directed graph, where each vertex is represented by a key in a mutable map, and
 * the value associated with each key is a mutable set of vertices representing the successors of that vertex.
 *
 * @tparam V the type of vertices in the graph
 */
class MapDirectedGraph[V] extends DirectedUnweightedGraph[V] {
  private val succs = mutable.Map[V, mutable.Set[V]]()

  override def addVertex(vertex: V): Boolean = if (!containsVertex(vertex)) {
    succs(vertex) = mutable.Set[V]()
    true
  }
  else {
    false
  }

  override def containsVertex(vertex: V): Boolean = succs.get(vertex) match {
    case None => false
    case Some(_) => true
  }

  override def deleteVertex(vertex: V): Boolean = if (containsVertex(vertex)) {
    succs -= vertex
    for ((_, destinationSet) <- succs) {
      destinationSet remove vertex
    }
    true
  }
  else {
    false
  }

  override def vertices: immutable.Set[V] = {
    immutable.Set.empty ++ succs.keys
  }

  override def order: Int = succs.size


  override def addEdge(source: V, destination: V): Boolean = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs.")
    }
    if (containsEdge(source, destination)) {
      false
    } else {
      succs.get(source) match {
        case Some(set) if containsVertex(destination) => set += destination
          true
        case _ => false
      }
    }
  }

  override def addEdge(directedEdge: DirectedEdge[V]): Boolean = {
    val DirectedEdge(source, destination) = directedEdge
    addEdge(source, destination)
  }


  override def containsEdge(source: V, destination: V): Boolean = {
    succs.get(source) match {
      case None => false
      case Some(set) => set.contains(destination)
    }
  }

  override def containsEdge(directedEdge: DirectedEdge[V]): Boolean = {
    succs.get(directedEdge.source) match {
      case None => false
      case Some(set) => set.contains(directedEdge.destination)
    }
  }

  override def deleteEdge(source: V, destination: V): Boolean = {
    if (containsEdge(source, destination)) {
      succs.get(source) match {
        case Some(set) if containsVertex(destination) => set -= destination
          true
        case _ => false
      }
    } else {
      false
    }
  }

  override def deleteEdge(directedEdge: DirectedEdge[V]): Boolean = {
    val DirectedEdge(source, destination) = directedEdge
    deleteEdge(source, destination)
  }

  override def edges[Edge[X] >: DirectedEdge[X]]: immutable.Set[Edge[V]] = {
    var edgeSet = immutable.Set[Edge[V]]()
    for ((source, destinationSet) <- succs) {
      destinationSet.foreach(destination => edgeSet += DirectedEdge(source, destination)) //??
    }
    edgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    for ((_, destinationSet) <- succs) {
      sum += destinationSet.size
    }
    sum
  }


  override def successors(source: V): immutable.Set[V] = {
    succs.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(successorSet) => immutable.Set.empty ++ successorSet
    }
  }

  override def predecessors(destination: V): immutable.Set[V] = if (containsVertex(destination)) {
    var predecessorSet = immutable.Set[V]()
    for ((predecessor, destinationSet) <- succs) {
      if (destinationSet.contains(destination)) {
        predecessorSet = predecessorSet + predecessor
      }
    }
    predecessorSet
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }

  override def incidentsFrom[Edge[X] >: DirectedEdge[X]](source: V): immutable.Set[Edge[V]] = {
    succs.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(successorSet) => var edgeSet = immutable.Set[Edge[V]]()
        successorSet.foreach(successor => edgeSet += DirectedEdge(source, successor))
        edgeSet
    }
  }

  override def incidentsTo[Edge[X] >: DirectedEdge[X]](destination: V): immutable.Set[Edge[V]] = if (containsVertex(destination)) {
    var edgeSet = immutable.Set[Edge[V]]()
    for ((predecessor, destinationSet) <- succs) {
      if (destinationSet.contains(destination)) {
        edgeSet += DirectedEdge(predecessor, destination)
      }
    }
    edgeSet
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }

  override def outdegree(source: V): Int = {
    succs.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) => set.size
    }
  }

  override def indegree(destination: V): Int = if (containsVertex(destination)) {
    var sum: Int = 0
    for ((_, destinationSet) <- succs) {
      if (destinationSet.contains(destination)) {
        sum += 1
      }
    }
    sum
  }
  else {
    throw GraphException(s"Vertex $destination not found.")
  }

}
