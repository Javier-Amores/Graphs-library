package graph

import scala.collection.immutable

/**
 * A trait representing a graph.
 *
 * @tparam V the type of vertices in the graph
 * @tparam E the kind of edges in the graph
 */
trait Graph[V, E[_]] {
  /**
   * Adds a vertex to the graph.
   *
   * @param vertex the vertex to add
   */
  def addVertex(vertex: V): Unit

  /**
   * Deletes a vertex from the graph.
   *
   * @param vertex the vertex to delete
   */
  def deleteVertex(vertex: V): Unit

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
   * Returns the set of vertices that are successors of a given vertex.
   *
   * @param vertex he vertex whose successors to return
   * @return the set of vertices that are successors of the given vertex
   */
  def successors(vertex: V): immutable.Set[V]

  /**
   * Returns the degree of a given vertex (i.e., the number of edges incident to the vertex).
   *
   * @param vertex the vertex whose degree to return
   * @return the degree of the given vertex
   */
  def degree(vertex: V): Int

  /**
   * Adds an edge between two vertices to the graph as a side effect and returns the new edge.
   *
   * @param vertex1 the first vertex of the edge
   * @param vertex2 the second vertex of the edge
   * @return the edge that was added
   */
  def addEdge(vertex1: V, vertex2: V): E[V]

  /**
   * Adds an edge to the graph.
   *
   * @param edge the edge to add
   */
  def addEdge(edge: E[V]): Unit

  /**
   * Deletes an edge from the graph.
   *
   * @param edge the edge to delete
   */
  def deleteEdge(edge: E[V]): Unit

  /**
   * Checks if the graph contains an edge.
   *
   * @param edge the edge to check
   * @return true if the graph contains the edge, false otherwise
   */
  def containsEdge(edge: E[V]): Boolean

  /**
   * Returns the set of edges in the graph.
   *
   * @return the set of edges in the graph
   */
  def edges: immutable.Set[E[V]]

  /**
   * Returns the size of the graph (i.e., the number of edges in the graph)
   *
   * @return the size of the graph
   */
  def size: Int
}
