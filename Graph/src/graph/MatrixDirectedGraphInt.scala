package graph

import scala.collection.immutable

object MatrixDirectedGraphInt {
  def apply(maxOrder: Int): MatrixDirectedGraphInt = new MatrixDirectedGraphInt(maxOrder)
}

/**
 * Represents a directed graph with integer vertices and directed edges using an adjacency matrix.
 *
 * @param maxOrder maximum number of vertices that the graph can hold
 */
class MatrixDirectedGraphInt(maxOrder: Int) extends DirectedUnweightedGraph[Int] {
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

  override def vertices: immutable.Set[Int] = {
    included.zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  override def order: Int = {
    included.count(vertex => vertex)
  }


  override def addEdge(source: Int, destination: Int): Boolean = {
    checkRange(source)
    checkRange(destination)
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs.")
    }
    if (containsEdge(source, destination)) {
      false
    } else {
      matrix(source)(destination) = true
      true
    }
  }

  override def addEdge(directedEdge: DirectedEdge[Int]): Boolean = {
    val DirectedEdge(source, destination) = directedEdge
    addEdge(source, destination)
  }


  override def containsEdge(source: Int, destination: Int): Boolean = {
    checkRange(source)
    checkRange(destination)
    matrix(source)(destination)
  }

  override def containsEdge(directedEdge: DirectedEdge[Int]): Boolean = {
    val DirectedEdge(source, destination) = directedEdge
    containsEdge(source, destination)
  }

  override def deleteEdge(source: Int, destination: Int): Boolean = {
    checkRange(source)
    checkRange(destination)
    if (containsEdge(source, destination)) {
      matrix(source)(destination) = false
      true
    } else {
      false
    }
  }

  override def deleteEdge(directedEdge: DirectedEdge[Int]): Boolean = {
    val DirectedEdge(source, destination) = directedEdge
    deleteEdge(source, destination)
  }

  override def edges[Edge[X] >: DirectedEdge[X]]: immutable.Set[Edge[Int]] = {
    var edgeSet = immutable.Set[Edge[Int]]()
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

  override def size: Int = {
    var sum: Int = 0
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        sum += matrix(vertex).count(edge => edge)
      }
    }
    sum
  }


  override def successors(source: Int): immutable.Set[Int] = {
    checkRange(source)
    if (included(source)) {
      matrix(source).zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
    } else {
      throw GraphException(s"vertex $source not found.")
    }
  }

  override def predecessors(destination: Int): immutable.Set[Int] = {
    checkRange(destination)
    if (included(destination)) {
      var predecessorSet = immutable.Set[Int]()
      for (i <- 0 until maxOrder) {
        if (matrix(i)(destination)) {
          predecessorSet += i
        }
      }
      predecessorSet
    }
    else {
      throw GraphException(s"vertex $destination not found.")
    }
  }

  override def incidentsFrom[Edge[X] >: DirectedEdge[X]](source: Int): immutable.Set[Edge[Int]] = {
    checkRange(source)
    if (included(source)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      matrix(source).zipWithIndex.foreach { case (boolean, index) if boolean => edgeSet += DirectedEdge(source, index); case _ => }
      edgeSet
    } else {
      throw GraphException(s"vertex $source not found.")
    }
  }

  override def incidentsTo[Edge[X] >: DirectedEdge[X]](destination: Int): immutable.Set[Edge[Int]] = {
    checkRange(destination)
    if (included(destination)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      for (i <- 0 until maxOrder) {
        if (matrix(i)(destination)) {
          edgeSet += DirectedEdge(i, destination)
        }
      }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $destination not found.")
    }
  }

  override def outdegree(source: Int): Int = {
    checkRange(source)
    if (included(source)) {
      matrix(source).count(vertex => vertex)
    }
    else {
      throw GraphException(s"vertex $source not found.")
    }
  }

  override def indegree(destination: Int): Int = {
    checkRange(destination)
    if (included(destination)) {
      var sum: Int = 0
      for (i <- 0 until maxOrder) {
        if (matrix(i)(destination)) {
          sum += 1
        }
      }
      sum
    }
    else {
      throw GraphException(s"vertex $destination not found.")
    }
  }
}

