package graph

import scala.collection.immutable

/**
 * A trait representing an undirected graph, with or without weighted edges.
 *
 * @tparam V the type of vertices in the graph
 */
// any undirected graph, weighted or not
trait UndirectedGraph[V] extends Graph[V, Edge] {
  /**
   * Returns an immutable set of vertices adjacent to the specified vertex.
   *
   * @param vertex the vertex to search for
   * @return an immutable set of adjacent vertices
   */
  def adjacents(vertex: V): immutable.Set[V]

  override def successors(vertex: V): immutable.Set[V] = adjacents(vertex)

  override def predecessors(vertex: V): immutable.Set[V] = adjacents(vertex)

  /**
   * Returns an immutable set of edges incident on the specified vertex.
   *
   * @param vertex the vertex to search for
   * @tparam E the type of edges in the graph
   * @return an immutable set of edges incident on the specified vertex
   */
  def incidents[E[X] >: Edge[X]](vertex: V): immutable.Set[E[V]] =
    incidentsFrom(vertex)

  /**
   * Returns the degree of a given vertex (i.e., the number of edges incident to the vertex).
   *
   * @param vertex the vertex whose degree to return
   * @return the degree of the given vertex
   */
  def degree(vertex: V): Int
}
