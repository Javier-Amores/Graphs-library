package graph.shortestPath

import graph.traversal.{Cycle, DirectedCycleFinder}
import graph.{DirectedEdge, DirectedWeightedEdge, DirectedWeightedGraph, GraphException, IsWeightedEdge, MapDirectedWeightedGraph, WeightedGraph}

import scala.collection.mutable

/**
 * Represents a Bellman-Ford shortest path algorithm for a directed weighted graph.
 *
 * @param graph  The graph on which the algorithm is applied
 * @param source The source vertex from which to compute the shortest paths
 * @param ord    The ordering of weights used for comparisons
 * @tparam V The type of vertices in the graph
 * @tparam W The type of weights
 */
case class BellmanFordShortestPath[V, W: Numeric](graph: WeightedGraph[V, W], source: V)(implicit ord: Ordering[W]) extends ShortestPath[V, W, ({type E[X] = IsWeightedEdge[X, W]})#E] {

  private val distTo = mutable.Map[V, W]()
  private val edgeTo = mutable.Map[V, IsWeightedEdge[V, W]]()
  private var cycle: Iterable[IsWeightedEdge[V, W]] = Iterable[IsWeightedEdge[V, W]]()

  /**
   * Executes the Bellman-Ford shortest path algorithm.
   */
  private def BellmanFordSP(): Unit = {
    val queue = mutable.Queue[V]()
    var cost: Int = 0
    val order: Int = graph.order
    distTo(source) = Numeric[W].zero
    queue.enqueue(source)
    while (queue.nonEmpty && !hasNegativeCycle) {
      val vertex: V = queue.dequeue()
      for (edge <- graph.incidentsFrom(vertex)) {
        val destinationVertex = edge.vertex2
        distTo.get(destinationVertex) match {
          case None => distTo(destinationVertex) = Numeric[W].plus(distTo(vertex), edge.weight)
            edgeTo(destinationVertex) = edge
            if (!queue.contains(destinationVertex)) {
              queue.enqueue(destinationVertex)
            }
          case Some(distanceToDestination) => distTo.get(vertex) match {
            case Some(distanceToVertex) if ord.compare(distanceToDestination, Numeric[W].plus(distanceToVertex, edge.weight)) > 0 => distTo(destinationVertex) = Numeric[W].plus(distTo(vertex), edge.weight)
              edgeTo(destinationVertex) = edge
              if (!queue.contains(destinationVertex)) {
                queue.enqueue(destinationVertex)
              }
            case _ =>
          }
        }
        if (cost % order == 0) {
          cost += 1
          findNegativeCycle()
        } else {
          cost += 1
        }

      }

    }
  }

  BellmanFordSP()

  /**
   * Finds a negative cycle in the graph, if exists.
   */
  private def findNegativeCycle(): Unit = {
    val spt = MapDirectedWeightedGraph[V, W]()
    for (vertex <- graph.vertices) {
      edgeTo.get(vertex) match {
        case Some(edge) => spt.addVertex(edge.vertex1)
          spt.addVertex(edge.vertex2)
          spt.addEdge(edge.vertex1, edge.vertex2, edge.weight)
        case None =>
      }
    }
    val cf = DirectedCycleFinder[V](spt)
    cycle = cf.cycle().asInstanceOf[Iterable[DirectedWeightedEdge[V, W]]]

  }

  /**
   * Checks if there is a negative cycle in the graph.
   *
   * @return true if a negative cycle is found, false otherwise
   */
  def hasNegativeCycle: Boolean = cycle.nonEmpty

  /**
   * Returns a negative cycle in the graph (or an empty iterable if such cycle doesn't exist).
   *
   * @return An iterable of directed weighted edges representing the negative cycle
   */
  def negativeCycle: Iterable[IsWeightedEdge[V, W]] = cycle


  def distTo(vertex: V): Option[W] = {
    if (hasNegativeCycle) {
      throw GraphException(s"Input graph contains a negative cycle reachable from source node $source")
    }
    else {
      distTo.get(vertex)
    }
  }

  def hasPathTo(vertex: V): Boolean = {
    if (hasNegativeCycle) {
      false
    }
    else {
      distTo.get(vertex) match {
        case None => false
        case _ => true
      }
    }
  }


  def pathTo[Edge[X] >: IsWeightedEdge[X, W]](vertex: V): Iterable[Edge[V]] = {
    if (!hasPathTo(vertex)) {
      throw GraphException(s"There is no minimum path from $source to $vertex")
    }
    else {
      val path = new mutable.Stack[IsWeightedEdge[V, W]]()
      var edgeOption = edgeTo.get(vertex)
      while (edgeOption.isDefined) {
        val edge = edgeOption.get
        path.push(edge)
        edgeOption = edgeTo.get(edge.vertex1)
      }
      path.asInstanceOf[Iterable[Edge[V]]]
    }
  }
}
