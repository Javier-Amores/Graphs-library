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
class MapDirectedGraph[V] extends DirectedGraph[V, DirectedEdge] {
  private val succs = mutable.Map[V, mutable.Set[V]]()

  override def addVertex(vertex: V): Unit = if (!succs.contains(vertex)) {
    succs(vertex) = mutable.Set[V]()
  }
  else {
    throw GraphException(s"Vertex $vertex is already in the graph.")
  }

  override def deleteVertex(vertex: V): Unit = if (containsVertex(vertex)) {
    succs -= vertex
    for ((_, destinationSet) <- succs) {
      destinationSet remove vertex
    }
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def containsVertex(vertex: V): Boolean = succs.get(vertex) match {
    case None => false
    case Some(_) => true
  }

  override def vertices: immutable.Set[V] = {
    val immutableVertices = immutable.Set.empty ++ succs.keys
    immutableVertices
  }

  override def order: Int = succs.size

  override def successors(vertex: V): immutable.Set[V] = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(successorSet) => immutable.Set.empty ++ successorSet
    }
  }

  override def predecessors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    var predecessorSet = immutable.Set[V]()
    for ((predecessor, destinationSet) <- succs) {
      if (destinationSet.contains(vertex)) {
        predecessorSet = predecessorSet + predecessor
      }
    }
    predecessorSet
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }


  override def degree(vertex: V): Int = indegree(vertex) + outdegree(vertex)

  override def indegree(vertex: V): Int = if (containsVertex(vertex)) {
    var sum: Int = 0
    for ((_, destinationSet) <- succs) {
      if (destinationSet.contains(vertex)) {
        sum += 1
      }
    }
    sum
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def outdegree(vertex: V): Int = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }


  override def addEdge(source: V, destination: V): DirectedEdge[V] = {
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val directedEdge = DirectedEdge(source, destination)
    if (containsEdge(directedEdge)) {
      throw GraphException(s"Edge $directedEdge is already in the graph.")
    }
    succs.get(source) match {
      case None => throw GraphException(s"Vertex $source not found.")
      case Some(set) if containsVertex(destination) => set += destination
        directedEdge
      case _ => throw GraphException(s"Vertex $destination not found.")
    }
  }

  override def addEdge(edge: DirectedEdge[V]): Unit = {
    if (containsEdge(edge)) {
      throw GraphException(s"Edge $edge is already in the graph.")
    }
    if (edge.source == edge.destination) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }

    succs.get(edge.source) match {
      case None => throw GraphException(s"Vertex ${edge.source} not found.")
      case Some(set) if containsVertex(edge.destination) => set += edge.destination
      case _ => throw GraphException(s"Vertex ${edge.destination} not found.")
    }
  }

  override def deleteEdge(edge: DirectedEdge[V]): Unit = {
    if (!containsEdge(edge)) {
      throw GraphException(s"Edge $edge not found.")
    }
    succs.get(edge.source) match {
      case None => throw GraphException(s"Vertex ${edge.source} not found.")
      case Some(set) if containsVertex(edge.destination) => set -= edge.destination
      case _ => throw GraphException(s"Vertex ${edge.destination} not found.")
    }
  }


  override def containsEdge(edge: DirectedEdge[V]): Boolean = {
    succs.get(edge.source) match {
      case None => false
      case Some(set) => set.contains(edge.destination)
    }
  }

  override def edges: immutable.Set[DirectedEdge[V]] = {
    var edgeSet = immutable.Set[DirectedEdge[V]]()
    for ((source, destinationSet) <- succs) {
      destinationSet.foreach(destination => edgeSet += DirectedEdge(source, destination))
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
}
