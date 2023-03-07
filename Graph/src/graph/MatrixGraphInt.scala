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
      for (i <- 0 until maxOrder ) {
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
    matrix(vertex).zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  def degree(vertex: Int): Int = {
    checkRange(vertex)
    matrix(vertex).count(vertex => vertex)
  }

  private def checkLoop(i: Int, j: Int): Unit =
    if (i == j)
      throw GraphException(s"Self-loops are not allowed in simple graphs.")

  def addEdge(vertex1: Int, vertex2: Int): Edge[Int] = {
    checkRange(vertex1)
    checkRange(vertex2)
    checkLoop(vertex1, vertex2)
    if (!included(vertex1)) {throw GraphException(s"vertex $vertex1 not found.")}
    if (!included(vertex2)) {throw GraphException(s"vertex $vertex2 not found.")}
    val edge = Edge(vertex1, vertex2)
    if (containsEdge(edge)) {
      throw GraphException(s"$edge is already in the graph.")
    } else {
      matrix(vertex1)(vertex2) = true
      matrix(vertex2)(vertex1) = true
      edge
    }
  }

  def addEdge(edge: Edge[Int]): Unit = {
    checkRange(edge.vertex1)
    checkRange(edge.vertex2)
    checkLoop(edge.vertex1, edge.vertex2)
    if (!included(edge.vertex1)) {
      throw GraphException(s"vertex ${edge.vertex1} not found.")
    }
    if (!included(edge.vertex2)) {
      throw GraphException(s"vertex ${edge.vertex2} not found.")
    }
    if (containsEdge(edge)) {
      throw GraphException(s"Edge $edge is already in the graph.")
    } else {
      matrix(edge.vertex1)(edge.vertex2) = true
      matrix(edge.vertex2)(edge.vertex1) = true
    }
  }

  def deleteEdge(edge: Edge[Int]): Unit = {
    checkRange(edge.vertex1)
    checkRange(edge.vertex2)
    if (!included(edge.vertex1)) {
      throw GraphException(s"vertex ${edge.vertex1} not found.")
    }
    if (!included(edge.vertex2)) {
      throw GraphException(s"vertex ${edge.vertex2} not found.")
    }
    if (containsEdge(edge)) {
      matrix(edge.vertex1)(edge.vertex2) = false
      matrix(edge.vertex2)(edge.vertex1) = false
    } else {
      throw GraphException(s"Edge $edge not found.")
    }
  }

  def containsEdge(edge: Edge[Int]): Boolean = {
    matrix(edge.vertex1)(edge.vertex2)
  }

  def edges: immutable.Set[Edge[Int]] = {
    var edgeSet = immutable.Set[Edge[Int]]()
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).take(vertex).zipWithIndex.collect{case (boolean,index) => if (boolean) {edgeSet += Edge(index,vertex)}}
      }
    }
    edgeSet
  }


  def size: Int = {
    var sum : Int = 0
    for ( vertex <- 1 until  maxOrder) {
      if (included(vertex)) {
        sum += matrix(vertex).take(vertex).count(edge => edge)
      }
    }
    sum
  }


}