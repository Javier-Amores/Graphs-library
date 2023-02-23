package graph

import scala.collection.{immutable, mutable}

object MapDirectedGraph {
  def apply[V](): MapDirectedGraph[V] = new MapDirectedGraph()
}

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
    for ((_,destinationSet) <- succs) {
      destinationSet remove vertex
    }
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def containsVertex(vertex: V): Boolean = succs.contains(vertex)

  override def vertices: immutable.Set[V] = {
    val immutableVertices = immutable.Set.empty ++ succs.keySet
    immutableVertices
  }

  override def order: Int = succs.size

  override def successors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    val immutableSuccs = immutable.Set.empty ++ succs(vertex)
    immutableSuccs
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def predecessors(vertex: V): immutable.Set[V] = if (containsVertex(vertex)) {
    val predecessorSet = mutable.Set[V]()
    for ((predecessor,destinationSet) <- succs) {
      if (destinationSet.contains(vertex)) {
        predecessorSet += predecessor
      }
    }
    val immutablePredecessorSet = immutable.Set.empty ++ predecessorSet
    immutablePredecessorSet
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def degree(vertex: V): Int = indegree(vertex)+outdegree(vertex)

  override def indegree(vertex: V): Int = if (containsVertex(vertex)) {
    var sum : Int = 0
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

  override def outdegree(vertex: V): Int = if (containsVertex(vertex)) {
    succs(vertex).size
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def addEdge(source: V, destination: V): DirectedEdge[V] = ???

  override def addEdge(edge: DirectedEdge[V]): Unit = if (containsEdge(edge)) {
    throw GraphException(s"Edge $edge is already in the graph.")
  }
  else if (!containsVertex(edge.source)) {
    throw GraphException(s"Vertex ${edge.source} not found.")
  }
  else if (!containsVertex(edge.destination)) {
    throw GraphException(s"Vertex ${edge.destination} not found.")
  }
  else {
    succs(edge.source) += edge.destination
  }

  override def deleteEdge(edge: DirectedEdge[V]): Unit = if (!containsEdge(edge)) {
    throw GraphException(s"Edge $edge not found.")
  }
  else if (!containsVertex(edge.source)) {
    throw GraphException(s"Vertex ${edge.source} not found.")
  }
  else if (!containsVertex(edge.destination)) {
    throw GraphException(s"Vertex ${edge.destination} not found.")
  }
  else {
    succs(edge.source) -= edge.destination
  }

  override def containsEdge(edge: DirectedEdge[V]): Boolean = containsVertex(edge.source) && (succs(edge.source) contains edge.destination)

  override def edges: immutable.Set[DirectedEdge[V]] = {
    val edgeSet = mutable.Set[DirectedEdge[V]]()
    for ((source, _) <- succs) {
      succs(source).foreach(destination => edgeSet += DirectedEdge(source, destination))
    }
    val finalEdgeSet = immutable.Set.empty ++ edgeSet
    finalEdgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    for ((source, _) <- succs) {
      sum += succs(source).size
    }
    sum
  }
}
