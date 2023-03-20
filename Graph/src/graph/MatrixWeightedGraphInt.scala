package graph

import scala.collection.immutable
import scala.reflect.ClassTag

object MatrixWeightedGraphInt {
  def apply[W: ClassTag](maxOrder: Int): MatrixWeightedGraphInt[W] = new MatrixWeightedGraphInt(maxOrder)
}

/**
 * Represents a weighted graph with integer vertices using an adjacency matrix.
 *
 * @param maxOrder maximum number of vertices that the graph can hold
 * @tparam W the type of the weights associated with the edges
 */
class MatrixWeightedGraphInt[W: ClassTag](maxOrder: Int) extends UndirectedWeightedGraph[Int, W] {
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

  override def addEdge(vertex1: Int, vertex2: Int, weight: W): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    if (vertex1 == vertex2 || containsEdge(vertex1, vertex2)) {
      false
    } else {
      matrix(vertex1)(vertex2) = Some(weight)
      matrix(vertex2)(vertex1) = Some(weight)
      true
    }
  }

  override def addEdge(weightedEdge: WeightedEdge[Int, W]): Boolean = {
    val WeightedEdge(vertex1, vertex2, weight) = weightedEdge
    addEdge(vertex1, vertex2, weight)
  }

  override def containsEdge(vertex1: Int, vertex2: Int): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    matrix(vertex1)(vertex2) match {
      case Some(_) => true
      case None => false
    }
  }

  override def containsEdge(vertex1: Int, vertex2: Int, weight: W): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    matrix(vertex1)(vertex2) match {
      case Some(otherWeight) if weight == otherWeight => true
      case _ => false
    }
  }

  override def containsEdge(weightedEdge: WeightedEdge[Int, W]): Boolean = {
    val WeightedEdge(vertex1, vertex2, weight) = weightedEdge
    containsEdge(vertex1, vertex2, weight)
  }

  override def deleteEdge(vertex1: Int, vertex2: Int): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    if (containsEdge(vertex1, vertex2)) {
      matrix(vertex1)(vertex2) = None
      matrix(vertex2)(vertex1) = None
      true

    } else {
      false

    }
  }

  override def deleteEdge(vertex1: Int, vertex2: Int, weight: W): Boolean = {
    checkRange(vertex1)
    checkRange(vertex2)
    if (containsEdge(vertex1, vertex2, weight)) {
      matrix(vertex1)(vertex2) = None
      matrix(vertex2)(vertex1) = None
      true

    } else {
      false

    }
  }

  override def deleteEdge(weightedEdge: WeightedEdge[Int, W]): Boolean = {
    val WeightedEdge(vertex1, vertex2, weight) = weightedEdge
    deleteEdge(vertex1, vertex2, weight)
  }

  override def edges[Edge[X] >: WeightedEdge[X, W]]: immutable.Set[Edge[Int]] = {
    var edgeSet = immutable.Set[Edge[Int]]()
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).take(vertex).zipWithIndex.collect { case (Some(weight), index) =>
          edgeSet += WeightedEdge(index, vertex, weight)
        }
      }
    }
    edgeSet
  }


  override def vertices: immutable.Set[Int] = {
    included.zipWithIndex.collect { case (boolean, index) if boolean => index }.toSet
  }

  override def weightOfEdge(vertex1: Int, vertex2: Int): Option[W] = {
    checkRange(vertex1)
    checkRange(vertex2)
    matrix(vertex1)(vertex2)
  }

  override def size: Int = {
    var sum: Int = 0
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        for (i <- matrix(vertex).take(vertex)) {
          i match {
            case Some(_) => sum += 1
            case None =>
          }
        }
      }
    }
    sum
  }

  override def adjacents(vertex: Int): immutable.Set[Int] = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).zipWithIndex.collect { case (Some(_), index) => index
      }.toSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  /*
    override def incidents[Edge[X] >: WeightedEdge[X, W]](vertex: Int): immutable.Set[Edge[Int]] = {
      checkRange(vertex)
      if (included(vertex)) {
        var edgeSet = immutable.Set[Edge[Int]]()
        matrix(vertex).zipWithIndex.foreach { case (Some(weight), index) => edgeSet += WeightedEdge(vertex, index, weight) }
        edgeSet
      }
      else {
        throw GraphException(s"vertex $vertex not found.")
      }
    }
  */
  override def degree(vertex: Int): Int = {
    checkRange(vertex)
    if (included(vertex)) {
      var sum: Int = 0
      for (i <- matrix(vertex)) {
        i match {
          case Some(_) => sum += 1
          case None =>
        }
      }
      sum
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  override def incidentsFrom[Edge[X] >: WeightedEdge[X, W]](vertex: Int): immutable.Set[Edge[Int]] = {
    checkRange(vertex)
    if (included(vertex)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      matrix(vertex).zipWithIndex.foreach { case (Some(weight), index) => edgeSet += WeightedEdge(vertex, index, weight); case _ => }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  override def incidentsTo[Edge[X] >: WeightedEdge[X, W]](vertex: Int): immutable.Set[Edge[Int]] = {
    checkRange(vertex)
    if (included(vertex)) {
      var edgeSet = immutable.Set[Edge[Int]]()
      matrix(vertex).zipWithIndex.foreach { case (Some(weight), index) => edgeSet += WeightedEdge(index, vertex, weight); case _ => }
      edgeSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

}