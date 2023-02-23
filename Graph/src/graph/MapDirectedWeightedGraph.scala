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
      destinationAndWeightSet remove Pair(vertex,_)
    }
  }
  else {
    throw GraphException(s"Vertex $vertex not found.")
  }


  override def containsVertex(vertex: V): Boolean = succsAndWeights.contains(vertex)

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  override def successors(vertex: V): immutable.Set[V] = ???

  override def predecessors(vertex: V): immutable.Set[V] = ???

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

  override def predecessorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

  override def degree(vertex: V): Int = ???

  override def indegree(vertex: V): Int = ???

  override def outdegree(vertex: V): Int = ???

  override def addEdge(source: V, destination: V): DirectedWeightedEdge[V, W] = ???

  override def addEdge(source: V, destination: V, weight: W): DirectedWeightedEdge[V, W] = ???

  override def addEdge(edge: DirectedWeightedEdge[V, W]): Unit = ???

  override def deleteEdge(edge: DirectedWeightedEdge[V, W]): Unit = ???

  override def containsEdge(edge: DirectedWeightedEdge[V, W]): Boolean = ???

  override def edges: Set[DirectedWeightedEdge[V, W]] = ???

  override def size: Int = ???
}
