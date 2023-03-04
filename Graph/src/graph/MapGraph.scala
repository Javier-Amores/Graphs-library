package graph

import scala.collection.{immutable, mutable}

object MapGraph {
  def apply[V](): MapGraph[V] = new MapGraph()
}

/**
 * Represents a graph, where each vertex is represented by a key in a mutable map, and
 * the value associated with each key is a mutable set of vertices representing the successors of that vertex.
 *
 * @tparam V the type of vertices in the graph
 */
class MapGraph[V] extends Graph[V, Edge] {
  private val succs = mutable.Map[V, mutable.Set[V]]()


  override def addVertex(vertex: V): Unit = if (!containsVertex(vertex)) {
    succs(vertex) = mutable.Set[V]()
  }
  else {
    throw GraphException(s"Vertex $vertex is already in the graph.")
  }

  override def deleteVertex(vertex: V): Unit = {
    succs.remove(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(successorSet) => successorSet.foreach(successor => succs.get(successor) match {
        case Some(set) => set.remove(vertex)
      })
    }
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
      case Some(set) => immutable.Set.empty ++ set
    }
  }

  override def degree(vertex: V): Int = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }


  override def addEdge(vertex1: V, vertex2: V): Edge[V] = {
    if (vertex1 == vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    val edge = Edge(vertex1, vertex2)
    if (containsEdge(edge)) {
      throw GraphException(s"Edge $edge is already in the graph.")
    }
    (succs.get(vertex1), succs.get(vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex $vertex1 not found.")
      case (_, None) => throw GraphException(s"Vertex $vertex2 not found.")
      case (Some(set1), Some(set2)) => set1 += vertex2
        set2 += vertex1
        edge
    }
  }


  override def addEdge(edge: Edge[V]): Unit = {
    if (containsEdge(edge)) {
      throw GraphException(s"Edge $edge is already in the graph.")
    }
    if (edge.vertex1 == edge.vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs")
    }
    (succs.get(edge.vertex1), succs.get(edge.vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex ${edge.vertex1} not found.")
      case (_, None) => throw GraphException(s"Vertex ${edge.vertex2} not found.")
      case (Some(set1), Some(set2)) => set1 += edge.vertex2
        set2 += edge.vertex1
    }
  }

  override def deleteEdge(edge: Edge[V]): Unit = {
    if (!containsEdge(edge)) {
      throw GraphException(s"Edge $edge not found.")
    }
    (succs.get(edge.vertex1), succs.get(edge.vertex2)) match {
      case (None, _) => throw GraphException(s"Vertex ${edge.vertex1} not found.")
      case (_, None) => throw GraphException(s"Vertex ${edge.vertex2} not found.")
      case (Some(set1), Some(set2)) => set1 -= edge.vertex2
        set2 -= edge.vertex1
    }
  }


  override def containsEdge(edge: Edge[V]): Boolean = {
    succs.get(edge.vertex1) match {
      case None => false
      case Some(set) => set.contains(edge.vertex2)
    }
  }

  override def edges: Set[Edge[V]] = {
    var edgeSet = immutable.Set[Edge[V]]()
    val visited = mutable.Set[V]()
    for ((vertex1, successorsSet) <- succs) {
      (successorsSet diff visited).foreach(vertex2 => edgeSet += Edge(vertex1, vertex2))
      visited += vertex1
    }
    edgeSet
  }

  override def size: Int = {
    var sum: Int = 0
    val visited = mutable.Set[V]()
    for ((vertex, successorsSet) <- succs) {
      sum += (successorsSet diff visited).size
      visited += vertex
    }
    sum
  }
}
