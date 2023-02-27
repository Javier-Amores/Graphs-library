package graph

import scala.collection.immutable

/**
 * Trait representing a weighted graph.
 * @tparam V the type of vertices in the graph
 * @tparam W the type of the weights associated with the edges
 * @tparam WE the kind of the weighted edge
 */
trait WeightedGraph[V, W, WE[_, _]] extends Graph[V, ({type E[a] = WE[a, W]})#E] {
  /**
   * Adds a weighted edge between two vertices in the graph as a side effect and returns the new edge.
   * @param vertex1 the first vertex to connect
   * @param vertex2 the second vertex to connect
   * @param weight the weight of the edge between the two vertices
   * @return a weighted edge connecting the two vertices
   */
  def addEdge(vertex1: V, vertex2: V, weight: W): WE[V, W]

  /**
   * Returns the set of successors and weights for a given vertex in the graph.
   * @param vertex the vertex to get the successors and weights for
   * @return an immutable set of tuples containing the vertex's successors and their corresponding weights
   */
  def successorsAndWeights(vertex: V): immutable.Set[(V, W)]
}
