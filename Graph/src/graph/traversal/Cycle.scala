package graph.traversal

import graph._

case class Cycle[V](graph:Graph[V,IsEdge]){
  private val vertices = graph.vertices
  private var notVisited = vertices
  private var cycle: Boolean = false

  for (vertex <- vertices) {
    if (notVisited.contains(vertex)) {
      dfs(vertex,vertex)
    }
  }

  private def dfs(vertex1: V,vertex2:V): Unit = {
    notVisited -= vertex1
    for (successor <- graph.successors(vertex1)) {
      if (notVisited.contains(successor)) {
        dfs(successor,vertex1)
      }
      else if (successor != vertex2) {
        cycle = true
      }
    }
  }

  def hasCycle:Boolean = cycle
}
