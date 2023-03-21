package graph

import scala.collection.{immutable, mutable}

object MatrixGraphInt {
  def apply(maxOrder: Int): MatrixGraphInt = new MatrixGraphInt(maxOrder)
}

/**
 * Represents a graph with integer vertices using an adjacency matrix to represent edges.
 *
 * @param maxOrder maximum number of vertices that the graph can hold
 */
class MatrixGraphInt(maxOrder: Int) extends UndirectedUnweightedGraph[Int] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  private val matrix = Array.ofDim[Boolean](maxOrder, maxOrder)

  /**
   * Throws an exception if the specified vertex is not within the valid range of vertices.
   *
   * @param i the vertex to check
   */
  private def checkRange(i: Int): Unit =
    if (!(0 <= i && i < maxOrder))
      throw GraphException(s"Vertex $i cannot be included in graph. Order is $maxOrder")

  override def addVertex(vertex: Int): Boolean = {
    checkRange(vertex)
    if (included(vertex)) {
      false
    } else {
      included(vertex) = true
      true
    }
  }

  override def containsVertex(vertex: Int): Boolean = {
    checkRange(vertex)
    included(vertex)
  }

  override def deleteVertex(vertex: Int): Boolean = {
    checkRange(vertex)
    if (included(vertex)) {
      included(vertex) = false
      matrix(vertex) = Array.fill(maxOrder)(false)
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) = false
      }
      true
    } else {
      false
    }
  }

  override def order: Int = {
    included.count(vertex => vertex)
  }

  override def addEdge(vertex1: Int, vertex2: Int): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    if (vertex1 == vertex2) {
      throw GraphException("Self-loops are not allowed in simple graphs.")
    }
    if (containsEdge(vertex1, vertex2)) {
      false
    } else {
      matrix(vertex1)(vertex2) = true
      matrix(vertex2)(vertex1) = true
      true
    }
  }

  override def addEdge(edge: Edge[Int]): Boolean = {
    val Edge(vertex1, vertex2) = edge
    addEdge(vertex1, vertex2)
  }

  override def containsEdge(vertex1: Int, vertex2: Int): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    matrix(vertex1)(vertex2)
  }

  override def containsEdge(edge: Edge[Int]): Boolean = {
    val Edge(vertex1, vertex2) = edge
    containsEdge(vertex1, vertex2)
  }

  override def deleteEdge(vertex1: Int, vertex2: Int): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    if (containsEdge(vertex1, vertex2)) {
      matrix(vertex1)(vertex2) = false
      matrix(vertex2)(vertex1) = false
      true
    } else {
      false
    }
  }

  override def deleteEdge(edge: Edge[Int]): Boolean = {
    val Edge(vertex1, vertex2) = edge
    deleteEdge(vertex1, vertex2)
  }

  override def edges[E[X] >: Edge[X]]: immutable.Set[E[Int]] = {
    var edgeSet = immutable.Set[E[Int]]()
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).take(vertex).zipWithIndex.collect { case (boolean, index) => if (boolean) {
          edgeSet += Edge(index, vertex)
        }
        }
      }
    }
    edgeSet
  }

  override def vertices: immutable.Set[Int] = {
    included.zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  override def size: Int = {
    var sum: Int = 0
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        sum += matrix(vertex).take(vertex).count(edge => edge)
      }
    }
    sum
  }


  override def adjacents(vertex: Int): immutable.Set[Int] = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  override def incidentsFrom[E[X] >: Edge[X]](vertex: Int): immutable.Set[E[Int]] = {
    checkRange(vertex)
    if (included(vertex)) {
      var edgeSet = immutable.Set[E[Int]]()
      matrix(vertex).zipWithIndex.foreach { case (boolean, index) if boolean => edgeSet += Edge(vertex, index); case _ => }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  override def incidentsTo[E[X] >: Edge[X]](vertex: Int): immutable.Set[E[Int]] = {
    checkRange(vertex)
    if (included(vertex)) {
      var edgeSet = immutable.Set[E[Int]]()
      matrix(vertex).zipWithIndex.foreach { case (boolean, index) if boolean => edgeSet += Edge(index, vertex); case _ => }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  override def degree(vertex: Int): Int = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).count(vertex => vertex)
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }


}
