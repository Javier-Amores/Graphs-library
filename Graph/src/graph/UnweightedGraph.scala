package graph

/**
 * A trait representing an unweighted graph, with or without directed edges.
 *
 * @tparam V the type of vertices in the graph
 */
// any unweighted graph, directed or not
trait UnweightedGraph[V] extends Graph[V, IsEdge] {
  /**
   * Adds an edge to the graph connecting the two specified vertices.
   *
   * @param vertex1 the first vertex to connect
   * @param vertex2 the second vertex to connect
   * @return true if the edge was successfully added, false otherwise
   */
  def addEdge(vertex1: V, vertex2: V): Boolean

  //def deleteEdge(vertex1: V, vertex2: V): Boolean
}
