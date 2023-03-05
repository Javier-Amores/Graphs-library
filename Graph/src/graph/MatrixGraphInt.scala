package graph

import scala.collection.immutable

object MatrixGraphInt {
  def apply(maxOrder: Int): MatrixGraphInt = new MatrixGraphInt(maxOrder)
}

class MatrixGraphInt(maxOrder: Int) extends Graph[Int, Edge] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  private val matrix = Array.ofDim[Boolean](maxOrder, maxOrder)

  private def checkRange(i: Int): Unit =
    if (!(0 <= i && i < maxOrder))
      throw new GraphException(s"Vertex $i cannot be included in graph. Order is $maxOrder")

  override def addVertex(vertex: Int): Unit = {
    checkRange(vertex)
    if (included(vertex)) {
      throw GraphException(s"Vertex $vertex is already in the graph.")
    } else {
      included(vertex) = true
    }
  }

  def deleteVertex(vertex: Int): Unit = ???

  def containsVertex(vertex: Int): Boolean = ???

  def vertices: immutable.Set[Int] = ???

  def order: Int = ???

  def successors(vertex: Int): immutable.Set[Int] = ???

  def degree(vertex: Int): Int = ???

  def addEdge(vertex1: Int, vertex2: Int): Edge[Int] = ???

  def addEdge(edge: Edge[Int]): Unit = ???

  def deleteEdge(edge: Edge[Int]): Unit = ???

  def containsEdge(edge: Edge[Int]): Boolean = ???

  def edges: immutable.Set[Edge[Int]] = ???

  def size: Int = ???


}