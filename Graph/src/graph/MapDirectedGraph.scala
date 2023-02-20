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

  override def deleteVertex(vertex: V): Unit = if (succs.contains(vertex)) {
    succs.subtractOne(vertex)
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }

  override def containsVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  override def successors(vertex: V): immutable.Set[V] = ???

  override def predecessors(vertex: V): immutable.Set[V] = ???

  override def degree(vertex: V): Int = ???

  override def indegree(vertex: V): Int = ???

  override def outdegree(vertex: V): Int = ???

  override def addEdge(source: V, destination: V): DirectedEdge[V] = ???

  override def addEdge(edge: DirectedEdge[V]): Unit = ???

  override def deleteEdge(edge: DirectedEdge[V]): Unit = ???

  override def containsEdge(edge: DirectedEdge[V]): Boolean = ???

  override def edges: immutable.Set[DirectedEdge[V]] = ???

  override def size: Int = ???
}
