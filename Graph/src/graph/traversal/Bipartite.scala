package graph.traversal

import graph.UndirectedGraph

import scala.collection.mutable


/**
 * Class for checking if a graph is bipartite.
 * @param graph the graph to be checked
 * @tparam V the type of the vertices in the graph
 */
case class Bipartite[V](graph: UndirectedGraph[V]){
  private val vertices = graph.vertices
  private var notVisited = vertices
  private val color = mutable.Map[V,Boolean]()
  vertices.foreach(vertex => color(vertex) = false)
  private var isTwoColorable: Boolean = true

  for (vertex <- vertices) {
    if (notVisited.contains(vertex)){
      dfs(vertex)
    }
  }

  /**
   * Performs a depth-first search to check if the graph is bipartite.
   * @param vertex the vertex to start the search from
   */
  private def dfs(vertex: V) :Unit ={
    notVisited -= vertex
    for (successor <- graph.adjacents(vertex)) {
      if (notVisited.contains(successor)) {
        color(successor) = !color(vertex)
        dfs(successor)
      }
      else if (color(successor)==color(vertex)) {
        isTwoColorable = false
      }
    }
  }

  /**
   * Checks if the graph if bipartite.
   * @return true if the graph is bipartite, false otherwise
   */
  def isBipartite:Boolean = isTwoColorable

}
