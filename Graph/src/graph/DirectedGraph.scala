package graph

import scala.collection.immutable

/**
 * A trait representing a directed graph.
 * @tparam V the type of vertices in the graph
 * @tparam E the kind of edges in the graph
 */
trait DirectedGraph[V, E[_]] extends Graph[V, E] {
  /**
   * Returns a set of the predecessors of a given vertex.
   * @param vertex the vertex whose predecessors to return
   * @return a set of the predecessors of the vertex
   */
  def predecessors(vertex: V): immutable.Set[V]

  /**
   * Returns the indegree of a given vertex.
   * @param vertex the vertex whose indegree to return
   * @return the indegree of the vertex
   */
  def indegree(vertex: V): Int

  /**
   * Returns the outdegree of a given vertex.
   * @param vertex the vertex whose outdegree to return
   * @return the outdegree of the vertex
   */
  def outdegree(vertex: V): Int
}
