package graph.traversal

import graph._

import scala.annotation.tailrec
import scala.collection.mutable


/**
 * Implements a breadth-first traversal of a graph, starting from a specified vertex.
 *
 * @param graph       the graph to traverse
 * @param startVertex the vertex to start the traversal from
 * @tparam V the type of the vertices in the graph
 */
class BreadthFirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends FirstTraversal[V](graph, startVertex) {
  protected val container: Container[IsEdge[V]] = new Queue[IsEdge[V]]
  protected val spanningTree: mutable.Map[V, V] = traverse()

  /**
   * Computes the shortest path distance between the starting vertex and the given vertex.
   *
   * @param vertex a vertex from the graph
   * @return An option with the shortest path distance if the nodes are connected, None otherwise
   */
  def shortestPathDistance(vertex:V):Option[Int] = shortestPathDistance(vertex, 0)


  @tailrec
  private def shortestPathDistance(vertex: V, distance: Int): Option[Int] = {
    spanningTree.get(vertex) match {
      case None => None
      case Some(parentVertex) => if (parentVertex == vertex) {
        Some(distance)
      }
      else {
        shortestPathDistance(parentVertex, distance+1)
      }
    }
  }
}