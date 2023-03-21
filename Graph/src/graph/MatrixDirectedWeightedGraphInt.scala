package graph

import scala.collection.immutable
import scala.reflect.ClassTag

object MatrixDirectedWeightedGraphInt {
  def apply[W: ClassTag](maxOrder: Int): MatrixDirectedWeightedGraphInt[W] = new MatrixDirectedWeightedGraphInt(maxOrder)
}


/**
 * Represents a directed weighted graph with integer vertices and directed weighted edges using an adjacency matrix.
 *
 * @param maxOrder maximum number of vertices that the graph can hold
 * @tparam W the type of weights in the graph
 */
class MatrixDirectedWeightedGraphInt[W: ClassTag](maxOrder: Int) extends DirectedWeightedGraph[Int, W] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  //Some(w) => The edge exists and has weight w
  //None => the edge doesn't exist
  private val matrix = {
    val nullMatrix = Array.ofDim[Option[W]](maxOrder, maxOrder)
    for (i <- 0 until maxOrder) {
      nullMatrix(i) = Array.fill(maxOrder)(None)
    }
    nullMatrix
  }


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
      matrix(vertex) = Array.fill(maxOrder)(None)
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) = None
      }
      true
    } else {
      false
    }
  }

  override def order: Int = {
    included.count(vertex => vertex)
  }

  override def addEdge(source: Int, destination: Int, weight: W): Boolean = {
    checkRange(source)
    checkRange(destination)
    if (source == destination) {
      throw GraphException("Self-loops are not allowed in simple graphs.")
    }
    if (containsEdge(source, destination)) {
      false
    } else {
      matrix(source)(destination) = Some(weight)
      true
    }
  }

  override def addEdge(directedWeightedEdge: DirectedWeightedEdge[Int, W]): Boolean = {
    val DirectedWeightedEdge(source, destination, weight) = directedWeightedEdge
    addEdge(source, destination, weight)
  }

  override def containsEdge(source: Int, destination: Int): Boolean = {
    checkRange(source)
    checkRange(destination)
    matrix(source)(destination) match {
      case Some(_) => true
      case None => false
    }
  }

  override def containsEdge(source: Int, destination: Int, weight: W): Boolean = {
    checkRange(source)
    checkRange(destination)
    matrix(source)(destination) match {
      case Some(otherWeight) if weight == otherWeight => true
      case _ => false
    }
  }

  override def containsEdge(directedWeightedEdge: DirectedWeightedEdge[Int, W]): Boolean = {
    val DirectedWeightedEdge(source, destination, weight) = directedWeightedEdge
    containsEdge(source, destination, weight)
  }

  override def deleteEdge(source: Int, destination: Int): Boolean = {
    checkRange(source)
    checkRange(destination)
    if (containsEdge(source, destination)) {
      matrix(source)(destination) = None
      true
    } else {
      false
    }
  }

  override def deleteEdge(source: Int, destination: Int, weight: W): Boolean = {
    checkRange(source)
    checkRange(destination)
    if (containsEdge(source, destination, weight)) {
      matrix(source)(destination) = None
      true
    } else {
      false
    }
  }

  override def deleteEdge(directedWeightedEdge: DirectedWeightedEdge[Int, W]): Boolean = {
    val DirectedWeightedEdge(source, destination, weight) = directedWeightedEdge
    deleteEdge(source, destination, weight)
  }

  override def edges[Edge[X] >: DirectedWeightedEdge[X, W]]: immutable.Set[Edge[Int]] = {
    var edgeSet = immutable.Set[Edge[Int]]()
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).zipWithIndex.collect { case (Some(weight), index) =>
          edgeSet += DirectedWeightedEdge(vertex, index, weight)
        }
      }
    }
    edgeSet
  }

  override def vertices: immutable.Set[Int] = {
    included.zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  override def weightOfEdge(source: Int, destination: Int): Option[W] = {
    checkRange(source)
    checkRange(destination)
    matrix(source)(destination)
  }

  override def size: Int = {
    var sum: Int = 0
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        for (i <- matrix(vertex)) {
          i match {
            case Some(_) => sum += 1
            case null => sum += 1
            case None =>
          }
        }
      }
    }
    sum
  }

  override def successors(source: Int): immutable.Set[Int] = {
    checkRange(source)
    if (included(source)) {
      matrix(source).zipWithIndex.collect { case (Some(_), index) => index
      }.toSet
    }
    else {
      throw GraphException(s"vertex $source not found.")
    }
  }

  override def predecessors(destination: Int): immutable.Set[Int] = {
    checkRange(destination)
    if (included(destination)) {
      var predecessorSet = immutable.Set[Int]()
      for (i <- 0 until maxOrder) {
        matrix(i)(destination) match {
          case Some(_) => predecessorSet += i
          case None =>
        }
      }
      predecessorSet
    }
    else {
      throw GraphException(s"vertex $destination not found.")
    }
  }

  override def incidentsFrom[Edge[X] >: DirectedWeightedEdge[X, W]](source: Int): immutable.Set[Edge[Int]] = {
    checkRange(source)
    if (included(source)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      matrix(source).zipWithIndex.foreach { case (Some(weight), index) => edgeSet += DirectedWeightedEdge(source, index, weight); case _ => }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $source not found.")
    }
  }

  override def incidentsTo[Edge[X] >: DirectedWeightedEdge[X, W]](destination: Int): immutable.Set[Edge[Int]] = {
    checkRange(destination)
    if (included(destination)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      for (i <- 0 until maxOrder) {
        matrix(i)(destination) match {
          case Some(weight) => edgeSet += DirectedWeightedEdge(i, destination, weight)
          case None =>
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
      var sum: Int = 0
      for (i <- matrix(source)) {
        i match {
          case Some(_) => sum += 1
          case None =>
        }
      }
      sum
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
        matrix(i)(destination) match {
          case Some(_) => sum += 1
          case None =>
        }
      }
      sum
    }
    else {
      throw GraphException(s"vertex $destination not found.")
    }
  }
}