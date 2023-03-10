package graph

import scala.collection.immutable

object MatrixDirectedGraphInt {
  def apply(maxOrder: Int): MatrixDirectedGraphInt = new MatrixDirectedGraphInt(maxOrder)
}

/**
 * Represents a directed graph with integer vertices and directed edges using an adjacency matrix.
 * @param maxOrder maximum number of vertices that the graph can hold
 */
class MatrixDirectedGraphInt(maxOrder: Int) extends DirectedGraph[Int, DirectedEdge] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  private val matrix = Array.ofDim[Boolean](maxOrder, maxOrder)

  /**
   * Throws an exception if the specified vertex is not within the valid range of vertices.
   * @param i the vertex to check
   */
  private def checkRange(i: Int): Unit =
    if (!(0 <= i && i < maxOrder))
      throw GraphException(s"Vertex $i cannot be included in graph. Order is $maxOrder")

  override def addVertex(vertex: Int): Unit = {
    checkRange(vertex)
    if (included(vertex)) {
      throw GraphException(s"Vertex $vertex is already in the graph.")
    } else {
      included(vertex) = true
    }
  }

  def deleteVertex(vertex: Int): Unit = {
    checkRange(vertex)
    if (included(vertex)) {
      included(vertex) = false
      matrix(vertex) = Array.fill(maxOrder)(false)
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) = false
      }
    } else {
      throw GraphException(s"Vertex $vertex not found.")
    }
  }

  def containsVertex(vertex: Int): Boolean = {
    checkRange(vertex)
    included(vertex)
  }

  def vertices: immutable.Set[Int] = {
    included.zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  def order: Int = {
    included.count(vertex => vertex)
  }

  def successors(vertex: Int): immutable.Set[Int] = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
    } else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def predecessors(vertex: Int): immutable.Set[Int] = {
    checkRange(vertex)
    if (included(vertex)) {
      var predecessorSet = immutable.Set[Int]()
      for (i <- 0 until maxOrder) {
        if (matrix(i)(vertex)) {
          predecessorSet += i
        }
      }
      predecessorSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def degree(vertex: Int): Int = indegree(vertex) + outdegree(vertex)

  def indegree(vertex: Int): Int = {
    checkRange(vertex)
    if (included(vertex)) {
      var sum: Int = 0
      for (i <- 0 until maxOrder) {
        if (matrix(i)(vertex)) {
          sum += 1
        }
      }
      sum
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def outdegree(vertex: Int): Int = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).count(vertex => vertex)
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  /**
   * Checks whether the source vertex and destination vertex of an edge are the same vertex (a self-loop).
   * If a self-loop is detected, a GraphException is thrown.
   *
   * @param i i The index of the source vertex
   * @param j The index of the destination vertex
   */
  private def checkLoop(i: Int, j: Int): Unit =
    if (i == j)
      throw GraphException(s"Self-loops are not allowed in simple graphs.")

  def addEdge(source: Int, destination: Int): DirectedEdge[Int] = {
    checkRange(source)
    checkRange(destination)
    checkLoop(source, destination)
    if (!included(source)) {
      throw GraphException(s"vertex $source}not found.")
    }
    if (!included(destination)) {
      throw GraphException(s"vertex $destination not found.")
    }
    val edge = DirectedEdge(source, destination)
    if (containsEdge(edge)) {
      throw GraphException(s"$edge is already in the graph.")
    } else {
      matrix(source)(destination) = true
      edge
    }
  }

  def addEdge(edge: DirectedEdge[Int]): Unit = {
    checkRange(edge.source)
    checkRange(edge.destination)
    checkLoop(edge.source, edge.destination)
    if (!included(edge.source)) {
      throw GraphException(s"vertex ${edge.source} not found.")
    }
    if (!included(edge.destination)) {
      throw GraphException(s"vertex ${edge.destination} not found.")
    }
    if (containsEdge(edge)) {
      throw GraphException(s"$edge is already in the graph.")
    } else {
      matrix(edge.source)(edge.destination) = true
    }
  }

  def deleteEdge(edge: DirectedEdge[Int]): Unit = {
    checkRange(edge.source)
    checkRange(edge.destination)
    if (!included(edge.source)) {
      throw GraphException(s"vertex ${edge.source} not found.")
    }
    if (!included(edge.destination)) {
      throw GraphException(s"vertex ${edge.destination} not found.")
    }
    if (containsEdge(edge)) {
      matrix(edge.source)(edge.destination) = false
    } else {
      throw GraphException(s"Edge $edge not found.")
    }
  }

  def containsEdge(edge: DirectedEdge[Int]): Boolean = {
    checkRange(edge.source)
    checkRange(edge.destination)
    matrix(edge.source)(edge.destination)
  }

  def edges: immutable.Set[DirectedEdge[Int]] = {
    var edgeSet = immutable.Set[DirectedEdge[Int]]()
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).zipWithIndex.collect { case (boolean, index) => if (boolean) {
          edgeSet += DirectedEdge(vertex, index)
        }
        }
      }
    }
    edgeSet
  }

  def size: Int = {
    var sum: Int = 0
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        sum += matrix(vertex).count(edge => edge)
      }
    }
    sum
  }

}
