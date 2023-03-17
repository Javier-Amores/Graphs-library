package graph

import scala.collection.immutable

/**
 * A trait representing a graph.
 *
 * @tparam V the type of vertices in the graph
 * @tparam E the kind of edges in the graph
 */
trait Graph[V, +E[_]] {
  /**
   * Adds a vertex to the graph.
   *
   * @param vertex the vertex to add
   * @return true if the vertex was added to the graph, false otherwise.
   */
  def addVertex(vertex: V): Boolean

  /**
   * Deletes a vertex from the graph.
   *
   * @param vertex the vertex to delete.
   * @return true if the vertex was deleted from the graph, false otherwise.
   */
  def deleteVertex(vertex: V): Boolean

  /**
   * Checks if the graph contains a vertex.
   *
   * @param vertex the vertex to check
   * @return true if the graph contains the vertex, false otherwise
   */
  def containsVertex(vertex: V): Boolean

  /**
   * Returns the set of vertices in the graph.
   *
   * @return the set of vertices in the graph
   */
  def vertices: immutable.Set[V]

  /**
   * Returns the order of the graph (i.e., the number of vertices in the graph).
   *
   * @return the order of the graph
   */
  def order: Int

  /**
   * Returns an immutable set of edges in the graph.
   *
   * @tparam Edge the type of edges in the graph
   * @return an immutable set of edges
   */
  def edges[Edge[X] >: E[X]]: immutable.Set[Edge[V]]

  /**
   * Returns the size of the graph (i.e., the number of edges in the graph)
   *
   * @return the size of the graph
   */
  def size: Int

  /**
   * Deletes the edge between two vertices.
   *
   * @param vertex1 the first vertex
   * @param vertex2 the second vertex
   * @return true if the edge was successfully deleted, false otherwise.
   */
  def deleteEdge(vertex1: V, vertex2: V): Boolean

  /**
   * Checks if the graph contains an edge.
   *
   * @param vertex1 the first vertex
   * @param vertex2 the second vertex
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(vertex1: V, vertex2: V): Boolean

  /**
   * Returns the set of vertices that are successors of a given vertex.
   *
   * @param vertex he vertex whose successors to return
   * @return the set of vertices that are successors of the given vertex
   */
  def successors(vertex: V): immutable.Set[V]

  /**
   * Returns a set of the predecessors of a given vertex.
   *
   * @param vertex the vertex whose predecessors to return
   * @return a set of the predecessors of the vertex
   */
  def predecessors(vertex: V): immutable.Set[V]

  /**
   * Returns an immutable set of edges where the specified vertex is the first vertex.
   *
   * @param vertex the vertex to search for
   * @tparam Edge the type of edges in the graph
   * @return an immutable set of edges where the specified vertex is the first vertex
   */
  // edges where vertex is first vertex
  def incidentsFrom[Edge[X] >: E[X]](vertex: V): immutable.Set[Edge[V]]

  /**
   * Returns an immutable set of edges where the specified vertex is the second vertex.
   *
   * @param vertex the vertex to search for
   * @tparam Edge the type of edges in the graph
   * @return an immutable set of edges where the specified vertex is the second vertex
   */
  // edges where vertex is second vertex
  def incidentsTo[Edge[X] >: E[X]](vertex: V): immutable.Set[Edge[V]]
}
