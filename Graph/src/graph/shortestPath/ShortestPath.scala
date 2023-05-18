package graph.shortestPath

/**
 * A trait representing a shortest path algorithm.
 *
 * @tparam V The type of vertices in the graph
 * @tparam W The type of weights
 * @tparam E The type of edges connecting vertices
 */
trait ShortestPath[V, W, +E[_]] {

  /**
   * Returns the shortest distance from the source vertex to the given vertex.
   *
   * @param v The destination vertex
   * @return An Option containing the shortest distance if a path exists, or None if no path is found
   */
  def distTo(v: V): Option[W]

  /**
   * Checks if there is a path from the source vertex to the given vertex.
   *
   * @param v The destination vertex
   * @return true if a path exists, false otherwise
   */
  def hasPathTo(v: V): Boolean

  /**
   * Returns an iterable of edges representing the shortest path from the source vertex to the given vertex.
   *
   * @param v The destination vertex
   * @tparam Edge The type of edges
   * @return An iterable collection of edges representing the shortest path
   */
  def pathTo[Edge[X] >: E[X]](v: V): Iterable[Edge[V]]

}
