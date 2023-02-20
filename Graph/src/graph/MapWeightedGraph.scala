package graph

import scala.collection.{immutable, mutable}

object MapWeightedGraph {
  def apply[V, W](): MapWeightedGraph[V, W] = new MapWeightedGraph()
}

class MapWeightedGraph[V, W] extends WeightedGraph[V, W, WeightedEdge] {
  private val succsAndWeights = mutable.Map[V, mutable.Set[Pair[V, W]]]()

  override def addVertex(vertex: V): Unit = ???

  override def deleteVertex(vertex: V): Unit = ???

  override def containsVertex(vertex: V): Boolean = ???

  override def vertices: immutable.Set[V] = ???

  override def order: Int = ???

  override def successors(vertex: V): immutable.Set[V] = ???

  override def successorsAndWeights(vertex: V): immutable.Set[(V, W)] = ???

  override def degree(vertex: V): Int = ???

  override def addEdge(vertex1: V, vertex2: V): WeightedEdge[V, W] = ???

  override def addEdge(vertex1: V, vertex2: V, weight: W): WeightedEdge[V, W] = ???

  override def addEdge(edge: WeightedEdge[V, W]): Unit = ???

  override def deleteEdge(edge: WeightedEdge[V, W]): Unit = ???

  override def containsEdge(edge: WeightedEdge[V, W]): Boolean = ???

  override def edges: Set[WeightedEdge[V, W]] = ???

  override def size: Int = ???
}
