package graph

import scala.collection.immutable

object MatrixDirectedGraphInt {
  def apply(maxOrder: Int): MatrixDirectedGraphInt = new MatrixDirectedGraphInt(maxOrder)
}

class MatrixDirectedGraphInt(maxOrder: Int) extends DirectedGraph[Int, DirectedEdge] {
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

  def predecessors(vertex: Int): immutable.Set[Int] = ???

  def degree(vertex: Int): Int = ???

  def indegree(vertex: Int): Int = ???

  def outdegree(vertex: Int): Int = ???

  def addEdge(vertex1: Int, vertex2: Int): DirectedEdge[Int] = ???

  def addEdge(edge: DirectedEdge[Int]): Unit = ???

  def deleteEdge(edge: DirectedEdge[Int]): Unit = ???

  def containsEdge(edge: DirectedEdge[Int]): Boolean = ???

  def edges: immutable.Set[DirectedEdge[Int]] = ???

  def size: Int = ???

}
