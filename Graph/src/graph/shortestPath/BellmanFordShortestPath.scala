package graph.shortestPath

import graph.traversal.Cycle
import graph.{DirectedWeightedEdge, DirectedWeightedGraph, GraphException, MapDirectedWeightedGraph}

import scala.collection.mutable

case class BellmanFordShortestPath[V, W: Numeric](graph: DirectedWeightedGraph[V, W], source: V)(implicit ord: Ordering[W]) extends ShortestPath[V, W, ({type E[X] = DirectedWeightedEdge[X, W]})#E] {

  private val distTo = mutable.Map[V, W]()
  private val edgeTo = mutable.Map[V, DirectedWeightedEdge[V, W]]()
  //private val cycle:mutable.Iterable[DirectedWeightedEdge[V, W]] = ???
  private var negativeCycle:Boolean = false

  private def BellmanFordSP(): Unit = {
    val onQueue = mutable.Set[V]()
    val queue = mutable.Queue[V]()
    var cost: Int = 0
    val order: Int = graph.order
    distTo(source) = null.asInstanceOf[W]
    queue.enqueue(source)
    onQueue += source
    while (queue.nonEmpty && !hasNegativeCycle) {
      val vertex: V = queue.dequeue()
      onQueue -= vertex
      for (edge <- graph.incidentsFrom(vertex)) {
        val destinationVertex = edge.destination
        distTo.get(destinationVertex) match {
          case None => distTo(destinationVertex) = implicitly[Numeric[W]].plus(distTo(vertex), edge.weight)
            edgeTo(destinationVertex) = edge
            if (!onQueue.contains(destinationVertex)) {
              queue.enqueue(destinationVertex)
              onQueue += destinationVertex
            }
          case Some(distanceToDestination) => distTo.get(vertex) match {
            case Some(distanceToVertex) if ord.compare(distanceToDestination, implicitly[Numeric[W]].plus(distanceToVertex, edge.weight)) > 0 => distTo(destinationVertex) = implicitly[Numeric[W]].plus(distTo(vertex), edge.weight)
              edgeTo(destinationVertex) = edge
              if (!onQueue.contains(destinationVertex)) {
                queue.enqueue(destinationVertex)
                onQueue += destinationVertex
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

  private def findNegativeCycle(): Unit = {
    val spt = MapDirectedWeightedGraph[V,W]()
    for (vertex <- graph.vertices) {
      edgeTo.get(vertex) match {
        case Some(edge) => spt.addVertex(edge.source)
                            spt.addVertex(edge.destination)
                            spt.addEdge(edge)
        case None =>
      }
    }
    val cf = Cycle[V](spt)
    negativeCycle = cf.hasCycle

  }

  def hasNegativeCycle: Boolean = negativeCycle

  //def negattiveCycle: mutable.Iterable[DirectedWeightedEdge[V, W]] = ???


  def distTo(vertex: V): Option[W] = distTo.get(vertex) match {
    case None => None
    case Some(distance) => Some(distance)
  }

  def hasPathTo(vertex: V): Boolean = distTo.get(vertex) match {
    case None => false
    case _ => true
  }

  def pathTo[Edge[X] >: DirectedWeightedEdge[X, W]](vertex: V): Iterable[Edge[V]] = {
    if (!hasPathTo(vertex)) {
      throw GraphException(s"Vertex $vertex is not reachable from $source")
    }
    else {
      val path = new mutable.Stack[DirectedWeightedEdge[V, W]]()
      var edgeOption = edgeTo.get(vertex)
      while (edgeOption.isDefined) {
        val edge = edgeOption.get
        path.push(edge)
        edgeOption = edgeTo.get(edge.source)
      }
      path.asInstanceOf[Iterable[Edge[V]]]
    }
  }
}
