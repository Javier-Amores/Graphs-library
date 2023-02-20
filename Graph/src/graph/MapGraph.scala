package graph

import scala.collection.{immutable, mutable}

object MapGraph {
  def apply[V](): MapGraph[V] = new MapGraph()
}

class MapGraph[V] extends Graph[V, Edge] {
  private val succs = mutable.Map[V, mutable.Set[V]]()


  override def addVertex(vertex: V): Unit = if (!containsVertex(vertex)) {succs(vertex) = mutable.Set[V]()}
  else {throw  GraphException(s"Vertex $vertex is already in the graph.")}

  override def deleteVertex(vertex: V): Unit = if (containsVertex(vertex)) { val successors = succs.remove(vertex)
  successors.get.foreach(successor => succs(successor) remove vertex)}
  else {throw  GraphException(s"Vertex $vertex not found.")}

  override def containsVertex(vertex: V): Boolean = succs.contains(vertex)

  override def vertices: immutable.Set[V] = {val immutableVertices = immutable.Set.empty ++ succs.keySet
  immutableVertices}

  override def order: Int = succs.size

  override def successors(vertex: V): immutable.Set[V] = if (containsVertex(vertex))
  {val immutableSuccs = immutable.Set.empty ++ succs(vertex)
    immutableSuccs}
  else {throw  GraphException(s"Vertex $vertex not found.")}

  override def degree(vertex: V): Int = if (containsVertex(vertex)) {succs(vertex).size}
  else {throw  GraphException(s"Vertex $vertex not found.")}

  override def addEdge(vertex1: V, vertex2: V): Edge[V] = ???

  override def addEdge(edge: Edge[V]): Unit = if (containsEdge(edge)) {throw  GraphException(s"Edge $edge is already in the graph.")}
  else if (!containsVertex(edge.vertex1)) {throw  GraphException(s"Vertex ${edge.vertex1} not found.")}
  else if (!containsVertex(edge.vertex2)) {throw  GraphException(s"Vertex ${edge.vertex2} not found.")}
  else {succs(edge.vertex1) += edge.vertex2
        succs(edge.vertex2) += edge.vertex1}

  override def deleteEdge(edge: Edge[V]): Unit = if (!containsEdge(edge)) {throw  GraphException(s"Edge $edge not found.")}
  else if (!containsVertex(edge.vertex1)) {
    throw GraphException(s"Vertex ${edge.vertex1} not found.")
  }
  else if (!containsVertex(edge.vertex2)) {
    throw GraphException(s"Vertex ${edge.vertex2} not found.")
  }
  else {
    succs(edge.vertex1) -= edge.vertex2
    succs(edge.vertex2) -= edge.vertex1
  }

  override def containsEdge(edge: Edge[V]): Boolean = (containsVertex(edge.vertex1) && (successors(edge.vertex1) contains edge.vertex2))

  override def edges: Set[Edge[V]] = ???

  override def size: Int = ???
}
