package graph

import scala.collection.immutable

trait DirectedGraph[V, E[_]] extends Graph[V, E] {
  def predecessors(vertex: V): immutable.Set[V]

  def indegree(vertex: V): Int

  def outdegree(vertex: V): Int
}
