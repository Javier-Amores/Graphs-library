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

  def degree(vertex: Int): Int = {
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

  private def checkLoop(i: Int, j: Int): Unit =
    if (i == j)
      throw GraphException(s"Self-loops are not allowed in simple graphs.")

  def addEdge(vertex1: Int, vertex2: Int): WeightedEdge[Int, W] = {
    checkRange(vertex1)
    checkRange(vertex2)
    checkLoop(vertex1, vertex2)
    if (!included(vertex1)) {
      throw GraphException(s"vertex $vertex1 not found.")
    }
    if (!included(vertex2)) {
      throw GraphException(s"vertex $vertex2 not found.")
    }
    val edge = WeightedEdge(vertex1, vertex2, null.asInstanceOf[W])
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(vertex1, vertex2)} is already in the graph.")
    } else {
      matrix(vertex1)(vertex2) = null
      matrix(vertex2)(vertex1) = null
      edge
    }
  }

  def addEdge(vertex1: Int, vertex2: Int, weight: W): WeightedEdge[Int, W] = {
    checkRange(vertex1)
    checkRange(vertex2)
    checkLoop(vertex1, vertex2)
    if (!included(vertex1)) {
      throw GraphException(s"vertex $vertex1 not found.")
    }
    if (!included(vertex2)) {
      throw GraphException(s"vertex $vertex2 not found.")
    }
    val edge = WeightedEdge(vertex1, vertex2, weight)
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(vertex1, vertex2)} is already in the graph.")
    } else {
      matrix(vertex1)(vertex2) = Some(weight)
      matrix(vertex2)(vertex1) = Some(weight)
      edge
    }
  }

  def addEdge(edge: WeightedEdge[Int, W]): Unit = {
    checkRange(edge.vertex1)
    checkRange(edge.vertex2)
    checkLoop(edge.vertex1, edge.vertex2)
    if (!included(edge.vertex1)) {
      throw GraphException(s"vertex ${edge.vertex1} not found.")
    }
    if (!included(edge.vertex2)) {
      throw GraphException(s"vertex ${edge.vertex2} not found.")
    }
    if (containsEdgeAnyWeight(edge)) {
      throw GraphException(s"${Edge(edge.vertex1, edge.vertex2)} is already in the graph.")
    } else {
      matrix(edge.vertex1)(edge.vertex2) = Some(edge.weight)
      matrix(edge.vertex2)(edge.vertex1) = Some(edge.weight)
    }
  }

  def deleteEdge(edge: WeightedEdge[Int, W]): Unit = {
    checkRange(edge.vertex1)
    checkRange(edge.vertex2)
    if (!included(edge.vertex1)) {
      throw GraphException(s"vertex ${edge.vertex1} not found.")
    }
    if (!included(edge.vertex2)) {
      throw GraphException(s"vertex ${edge.vertex2} not found.")
    }
    if (containsEdge(edge)) {
      matrix(edge.vertex1)(edge.vertex2) = None
      matrix(edge.vertex2)(edge.vertex1) = None

    } else {
      throw GraphException(s"edge $edge not found.")

    }
  }

  def containsEdge(edge: WeightedEdge[Int, W]): Boolean = {
    checkRange(edge.vertex1)
    checkRange(edge.vertex2)
    matrix(edge.vertex1)(edge.vertex2) match {
      case Some(weight) if weight == edge.weight => true
      case null if null.asInstanceOf[W] == edge.weight => true
      case _ => false

    }
  }

  private def containsEdgeAnyWeight(edge: Edge[Int]): Boolean = {
    matrix(edge.vertex1)(edge.vertex2) match {
      case Some(_) => true
      case null => true
      case None => false
    }
  }

  def edges: immutable.Set[WeightedEdge[Int, W]] = {
    var edgeSet = immutable.Set[WeightedEdge[Int, W]]()
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        matrix(vertex).take(vertex).zipWithIndex.collect { case (Some(weight), index) =>
          edgeSet += WeightedEdge(index, vertex, weight)

        case (null, index) => edgeSet += WeightedEdge(index, vertex, null.asInstanceOf[W])
        }
      }
    }
    edgeSet
  }


  def size: Int = {
    var sum: Int = 0
    for (vertex <- 1 until maxOrder) {
      if (included(vertex)) {
        for (i <- matrix(vertex).take(vertex)) {
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
