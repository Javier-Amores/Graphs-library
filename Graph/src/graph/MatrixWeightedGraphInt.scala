package graph

import scala.collection.immutable
import scala.reflect.ClassTag

object MatrixWeightedGraphInt {
  def apply[W: ClassTag](maxOrder: Int): MatrixWeightedGraphInt[W] = new MatrixWeightedGraphInt(maxOrder)
}

class MatrixWeightedGraphInt[W: ClassTag](maxOrder: Int) extends WeightedGraph[Int, W, WeightedEdge] {
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

  def successorsAndWeights(vertex: Int): immutable.Set[(Int, W)] = ???

  def degree(vertex: Int): Int = ???

  def addEdge(vertex1: Int, vertex2: Int): WeightedEdge[Int, W] = ???

  def addEdge(vertex1: Int, vertex2: Int, weight: W): WeightedEdge[Int, W] = ???

  def addEdge(edge: WeightedEdge[Int, W]): Unit = ???

  def deleteEdge(edge: WeightedEdge[Int, W]): Unit = ???

  def containsEdge(edge: WeightedEdge[Int, W]): Boolean = ???

  def edges: immutable.Set[WeightedEdge[Int, W]] = ???

  def size: Int = ???


}
