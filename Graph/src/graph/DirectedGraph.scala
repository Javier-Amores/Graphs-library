package graph


/**
 * trait representing a directed graph, with or without weighted edges.
 *
 * @tparam V the type of vertices in the graph
 */
// any directed graph, weighted or not
trait DirectedGraph[V] extends Graph[V, DirectedEdge] {

  /**
   * Returns the indegree of the specified vertex.
   *
   * @param destination the vertex to search for
   * @return the indegree of the specified vertex
   */
  def indegree(destination: V): Int


  /**
   * Returns the outdegree of the specified vertex.
   *
   * @param source the vertex to search for
   * @return the outdegree of the specified vertex
   */
  def outdegree(source: V): Int

  /**
   * Returns the total degree of the specified vertex, which is the sum of its indegree and outdegree.
   *
   * @param vertex the vertex to search for
   * @return the total degree of the specified vertex
   */
  def degree(vertex: V): Int =
    indegree(vertex) + outdegree(vertex)

  // def adjacents(vertex:V): immutable.Set[V] = successors ++ predecessors ??
}
