package graph.traversal

import scala.collection.mutable


/**
 * Represents a traversal algorithm
 *
 * @tparam V the type of the vertices in the graph
 */
trait Traversal[V] {
  /**
   * Checks if a vertex is reachable from the starting vertex of the traversal.
   *
   * @param vertex the vertex to check for reachability
   * @return true if the vertex is reachable, false otherwise
   */
  def isReachable(vertex: V): Boolean

  /**
   * Returns the path from the starting vertex to the specified vertex.
   *
   * @param vertex the vertex to find a path to
   * @return an optional list of vertices representing the path from the starting vertex to the specified vertex, or None if there is no path
   */
  def pathTo(vertex: V): Option[List[V]]

  /**
   * Returns a spanning tree of the graph.
   *
   * @return a mutable map representing the spanning tree
   */
  def getSpanningTree: mutable.Map[V, mutable.Set[V]]
}
