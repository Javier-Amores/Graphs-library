package graph.traversal

import graph.UndirectedGraph

import scala.collection.mutable


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

  def isBipartite():Boolean = isTwoColorable

}
