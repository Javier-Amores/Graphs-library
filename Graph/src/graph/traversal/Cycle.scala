package graph.traversal

import graph._

import scala.collection.mutable

/**
 * Class for checking if a graph is acyclic
 *
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class Cycle[V](graph: Graph[V, IsEdge]) {
  private var cycle: Boolean = false
  private var notVisited = graph.vertices

  private def main(): Unit = {
    val vertices = notVisited.iterator
    while (vertices.hasNext && !cycle) {
      val vertex = vertices.next()
      if (notVisited.contains(vertex)) {
        dfs(vertex, vertex)
      }
    }
  }

  main()

  /**
   * Depth-first search algorithm that searches for cycles in the graph.
   *
   * @param vertex1 the current vertex being visited
   * @param vertex2 the parent vertex of the current vertex
   */
  private def dfs(vertex1: V, vertex2: V): Unit = {
    notVisited -= vertex1
    val successors = graph.successors(vertex1).iterator
    while (successors.hasNext && !cycle) {
      val successor = successors.next()
      if (notVisited.contains(successor)) {
        dfs(successor, vertex1)
      }
      else if (successor != vertex2) {
        cycle = true
      }
    }
  }

  /**
   * Checks if the graph has at least one cycle.
   *
   * @return true if the graph has at least one cycle, false otherwise
   */
  def hasCycle: Boolean = cycle
}
