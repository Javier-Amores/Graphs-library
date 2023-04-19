package graph.traversal

import graph.UndirectedGraph

import scala.collection.mutable


/**
 * Class for checking if a graph is bipartite.
 *
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class Bipartite[V](graph: UndirectedGraph[V]) {
  private val vertices = graph.vertices.iterator
  private val color = mutable.Map[V, Boolean]()
  private var isTwoColorable: Boolean = true

  while (vertices.hasNext && isTwoColorable) {
    val vertex = vertices.next
    color.get(vertex) match {
      case None => dfs(vertex)
      case _ =>
    }
  }


  /**
   * Performs a depth-first search to check if the graph is bipartite.
   *
   * @param vertex the vertex to start the search from
   */
  private def dfs(vertex: V): Unit = {
    val vertexColor = color.getOrElseUpdate(vertex, false)
    val adjacents = graph.adjacents(vertex).iterator
    while (adjacents.hasNext && isTwoColorable) {
      val successor = adjacents.next()
      color.get(successor) match {
        case None => color(successor) = !vertexColor
          dfs(successor)
        case Some(c) if c == color(vertex) => isTwoColorable = false
        case _ =>
      }
    }
  }

  /**
   * Checks if the graph if bipartite.
   *
   * @return true if the graph is bipartite, false otherwise
   */
  def isBipartite: Boolean = isTwoColorable

}
