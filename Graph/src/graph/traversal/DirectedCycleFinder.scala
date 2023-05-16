package graph.traversal

import graph.{DirectedEdge, DirectedGraph, DirectedWeightedEdge, IsDirectedEdge}

import scala.collection.mutable

case class DirectedCycleFinder[V](graph: DirectedGraph[V]) {
  private var notVisited = graph.vertices
  private val edgeTo = mutable.Map[V, DirectedEdge[V]]()
  private val stackCycle = mutable.Stack[DirectedEdge[V]]()
  private val onStack = mutable.Set[V]()

  private def main(): Unit = {
    val vertices = notVisited.iterator
    while (vertices.hasNext && !hasCycle) {
      val vertex = vertices.next()
      if (notVisited.contains(vertex)) {
        dfs(vertex)
      }
    }
  }

  main()


  private def dfs(vertex:V):Unit = {
    onStack+=vertex
    notVisited -= vertex
    val incidentEdges = graph.incidentsFrom(vertex).iterator
    while (incidentEdges.hasNext && !hasCycle) {
      val edge = incidentEdges.next()
      val successor = edge.destination
      if (notVisited.contains(successor)){
        edgeTo(successor) = edge
        dfs(successor)
      } else if (onStack.contains(successor)) {
        var edgeCycle:DirectedEdge[V] = edge
        while (edgeCycle.source!=successor) {
          stackCycle.push(edgeCycle)
          edgeCycle = edgeTo(edgeCycle.source)
        }
        stackCycle.push(edgeCycle)
      }
    }
    onStack-= vertex
      }

  def hasCycle:Boolean = {
    stackCycle.nonEmpty
  }

  def cycle():Iterable[DirectedEdge[V]] = stackCycle


}
