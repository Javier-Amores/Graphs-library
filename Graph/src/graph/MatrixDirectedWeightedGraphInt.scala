package graph

import scala.collection.immutable
import scala.reflect.ClassTag

object MatrixDirectedWeightedGraphInt {
  def apply[W: ClassTag](maxOrder: Int): MatrixDirectedWeightedGraphInt[W] = new MatrixDirectedWeightedGraphInt(maxOrder)
}


/**
 * Represents a directed weighted graph with integer vertices and directed weighted edges using an adjacency matrix.
 * @param maxOrder maximum number of vertices that the graph can hold
 * @tparam W the type of weights in the graph
 */
class MatrixDirectedWeightedGraphInt[W: ClassTag](maxOrder: Int) extends DirectedWeightedGraph[Int, W, DirectedWeightedEdge] {
  // included(i) == true if vertex i was added to graph
  private val included = Array.fill(maxOrder)(false)
  // adjacency matrix for representing edges
  //Some(w) => The edge exists and has weight w
  //null => The edge exists but has no weight.
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
      matrix(vertex) = Array.fill(maxOrder)(None)
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) = None
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
      matrix(vertex).zipWithIndex.collect { case (Some(_), index) => index
      case (null, index) => index
      }.toSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def predecessors(vertex: Int): immutable.Set[Int] = {
    checkRange(vertex)
    if (included(vertex)) {
      var predecessorSet = immutable.Set[Int]()
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) match {
          case Some(_) => predecessorSet += i
          case null => predecessorSet += i
          case None =>
        }
      }
      predecessorSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def successorsAndWeights(vertex: Int): immutable.Set[(Int, W)] = {
    checkRange(vertex)
    if (included(vertex)) {
      matrix(vertex).zipWithIndex.collect { case (Some(weight), index) => (index, weight)
      case (null, index) => (index, null.asInstanceOf[W])
      }.toSet
    }
    else {
      throw GraphException(s"vertex $vertex not found.")
    }
  }

  def predecessorsAndWeights(vertex: Int): immutable.Set[(Int, W)] = {
    checkRange(vertex)
    if (included(vertex)) {
      var predecessorSet = immutable.Set[(Int, W)]()
      var predecessor: (Int, W) = (0, null.asInstanceOf[W])
      for (i <- 0 until maxOrder) {
        matrix(i)(vertex) match {
          case Some(weight) => predecessor = (i, weight)
            predecessorSet += predecessor
          case null => predecessor = (i, null.asInstanceOf[W])
            predecessorSet += predecessor
          case None =>
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
        matrix(i)(vertex) match {
          case Some(_) => sum += 1
          case null => sum += 1
          case None =>
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
      var sum: Int = 0
      for (i <- matrix(vertex)) {
        i match {
          case Some(_) => sum += 1
          case null => sum += 1
          case None =>
        }
      }
      sum
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

  def addEdge(source: Int, destination: Int): DirectedWeightedEdge[Int, W] = {
    checkRange(source)
    checkRange(destination)
    checkLoop(source, destination)
    if (!included(source)) {
      throw GraphException(s"vertex $source not found.")
    }
    if (!included(destination)) {
      throw GraphException(s"vertex $destination not found.")
    }
    val edge = DirectedWeightedEdge(source, destination, null.asInstanceOf[W])
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(source, destination)} is already in the graph.")
    } else {
      matrix(source)(destination) = null
      edge
    }
  }

  def addEdge(source: Int, destination: Int, weight: W): DirectedWeightedEdge[Int, W] = {
    checkRange(source)
    checkRange(destination)
    checkLoop(source, destination)
    if (!included(source)) {
      throw GraphException(s"vertex $source not found.")
    }
    if (!included(destination)) {
      throw GraphException(s"vertex $destination not found.")
    }
    val edge = DirectedWeightedEdge(source, destination, weight)
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(source, destination)} is already in the graph.")
    } else {
      matrix(source)(destination) = Some(weight)
      edge
    }
  }

  def addEdge(edge: DirectedWeightedEdge[Int, W]): Unit = {
    checkRange(edge.source)
    checkRange(edge.destination)
    checkLoop(edge.source, edge.destination)
    if (!included(edge.source)) {
      throw GraphException(s"vertex ${edge.source} not found.")
    }
    if (!included(edge.destination)) {
      throw GraphException(s"vertex ${edge.destination} not found.")
    }
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${DirectedEdge(edge.source, edge.destination)} is already in the graph.")
    } else {
      matrix(edge.source)(edge.destination) = Some(edge.weight)
    }
  }

  def deleteEdge(edge: DirectedWeightedEdge[Int, W]): Unit = {
    checkRange(edge.source)
    checkRange(edge.destination)
    if (!included(edge.source)) {
      throw GraphException(s"vertex ${edge.source} not found.")
    }
    if (!included(edge.destination)) {
      throw GraphException(s"vertex ${edge.destination} not found.")
    }
    if (containsEdge(edge)) {
      matrix(edge.source)(edge.destination) = None
    } else {
      throw GraphException(s"Edge $edge not found.")
    }
  }

  def containsEdge(edge: DirectedWeightedEdge[Int, W]): Boolean = {
    checkRange(edge.source)
    checkRange(edge.destination)
    matrix(edge.source)(edge.destination) match {
      case Some(weight) if weight == edge.weight => true
      case null if null.asInstanceOf[W] == edge.weight => true
      case _ => false
    }
  }

  /**
   * Checks if the graph contains an edge regardless of the weight.
   *
   * @param edge the edge to check
   * @return true if the graph contains the edge, false otherwise
   */
  private def containsEdgeAnyWeight(edge: DirectedEdge[Int]): Boolean = {
    matrix(edge.source)(edge.destination) match {
      case Some(_) => true
      case null => true
      case None => false
    }
  }

  def edges: immutable.Set[DirectedWeightedEdge[Int, W]] = {
    var edgeSet = immutable.Set[DirectedWeightedEdge[Int, W]]()
    for (vertex <- 0 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).zipWithIndex.collect { case (Some(weight), index) =>
          edgeSet += DirectedWeightedEdge(vertex, index, weight)

        case (null, index) => edgeSet += DirectedWeightedEdge(vertex, index, null.asInstanceOf[W])
        }
      }
    }
    edgeSet
  }

  def size: Int = {
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
}
