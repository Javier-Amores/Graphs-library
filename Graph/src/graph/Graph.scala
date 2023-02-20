package graph

import scala.collection.immutable

trait Graph[V, E[_]] {
  def addVertex(vertex: V): Unit

  def deleteVertex(vertex: V): Unit

  def containsVertex(vertex: V): Boolean

  def vertices: immutable.Set[V]

  def order: Int

  def successors(vertex: V): immutable.Set[V]

  def degree(vertex: V): Int

  def addEdge(vertex1: V, vertex2: V): E[V]

  def addEdge(edge: E[V]): Unit

  def deleteEdge(edge: E[V]): Unit

  def containsEdge(edge: E[V]): Boolean

  def edges: immutable.Set[E[V]]

  def size: Int
}
