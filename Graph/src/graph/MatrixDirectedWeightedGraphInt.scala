package graph

import scala.collection.immutable
import scala.reflect.ClassTag

object MatrixDirectedWeightedGraphInt {
  def apply[W: ClassTag](maxOrder: Int): MatrixDirectedWeightedGraphInt[W] = new MatrixDirectedWeightedGraphInt(maxOrder)
}


class MatrixDirectedWeightedGraphInt[W: ClassTag](maxOrder: Int) extends DirectedWeightedGraph[Int, W, DirectedWeightedEdge] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  private val matrix = Array.ofDim[W](maxOrder)


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

  def successorsAndWeights(vertex: Int): immutable.Set[(Int, W)] = ???

  def predecessorsAndWeights(vertex: Int): immutable.Set[(Int, W)] = ???

  def degree(vertex: Int): Int = ???

  def indegree(vertex: Int): Int = ???

  def outdegree(vertex: Int): Int = ???

  def addEdge(vertex1: Int, vertex2: Int): DirectedWeightedEdge[Int, W] = ???

  def addEdge(vertex1: Int, vertex2: Int, weight: W): DirectedWeightedEdge[Int, W] = ???

  def addEdge(edge: DirectedWeightedEdge[Int, W]): Unit = ???

  def deleteEdge(edge: DirectedWeightedEdge[Int, W]): Unit = ???

  def containsEdge(edge: DirectedWeightedEdge[Int, W]): Boolean = ???

  def edges: immutable.Set[DirectedWeightedEdge[Int, W]] = ???

  def size: Int = ???
}
