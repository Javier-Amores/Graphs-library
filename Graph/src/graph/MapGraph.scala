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
class MapGraph[V] extends UndirectedUnweightedGraph[V] {

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

  override def deleteVertex(vertex: V): Boolean = {
    succs.remove(vertex) match {
      case None => false
      case Some(successorSet) => successorSet.foreach(successor => succs.get(successor) match {
        case Some(set) => set.remove(vertex)
      })
        true
    }
  }

  override def order: Int = succs.size

  override def addEdge(vertex1: V, vertex2: V): Boolean = {
    if (vertex1 == vertex2) {
      return false
    }
    if (containsEdge(vertex1, vertex2)) {
      return false
    }
    (succs.get(vertex1), succs.get(vertex2)) match {
      case (Some(set1), Some(set2)) => set1 += vertex2
        set2 += vertex1
        true
      case _ => false
    }
  }

  override def addEdge(edge: Edge[V]): Boolean = {
    val Edge(vertex1, vertex2) = edge
    addEdge(vertex1, vertex2)
  }

  override def containsEdge(vertex1: V, vertex2: V): Boolean = {
    succs.get(vertex1) match {
      case None => false
      case Some(set) => set.contains(vertex2)
    }
  }

  override def containsEdge(edge: Edge[V]): Boolean = {
    succs.get(edge.vertex1) match {
      case None => false
      case Some(set) => set.contains(edge.vertex2)
    }
  }

  override def deleteEdge(vertex1: V, vertex2: V): Boolean = {
    if (!containsEdge(vertex1, vertex2)) {
      return false
    }
    (succs.get(vertex1), succs.get(vertex2)) match {
      case (Some(set1), Some(set2)) => set1 -= vertex2
        set2 -= vertex1
        true
      case _ => false
    }
  }

  override def deleteEdge(edge: Edge[V]): Boolean = {
    val Edge(vertex1, vertex2) = edge
    deleteEdge(vertex1, vertex2)
  }

  override def edges[E[X] >: Edge[X]]: immutable.Set[E[V]] = {
    var edgeSet = immutable.Set[E[V]]()
    val visited = mutable.Set[V]()
    for ((vertex1, successorsSet) <- succs) {
      (successorsSet diff visited).foreach(vertex2 => edgeSet += Edge(vertex1, vertex2))
      visited += vertex1
    }
    edgeSet
  }

  override def vertices: immutable.Set[V] = {
    immutable.Set.empty ++ succs.keys
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


  override def adjacents(vertex: V): immutable.Set[V] = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set
    }
  }


  /*
  override def incidents[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => immutable.Set.empty ++ set.map(adjacentVertex => Edge(vertex, adjacentVertex))
    }
  }*/

  //??
  override def incidentsFrom[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => var edgeSet = immutable.Set[E[V]]()
        set.foreach(adjacentVertex => edgeSet += Edge(vertex, adjacentVertex))
        edgeSet
    }
  }

  //??
  override def incidentsTo[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => var edgeSet = immutable.Set[E[V]]()
        set.foreach(adjacentVertex => edgeSet += Edge(adjacentVertex, vertex))
        edgeSet
    }
  }

  override def degree(vertex: V): Int = {
    succs.get(vertex) match {
      case None => throw GraphException(s"Vertex $vertex not found.")
      case Some(set) => set.size
    }
  }
}
