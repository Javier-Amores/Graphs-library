package graph.MST

import graph._
import graph.traversal.DFTConnected

import scala.collection.mutable
import scala.math.Numeric.Implicits._
import scala.util.control.Breaks._

/**
 * A class that implements the Prim's algorithm to find the minimum spanning tree of an undirected weighted graph.
 *
 * @param graph the graph for which to find the minimum spanning tree
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights associated with the edges in the graph
 */
case class LazyPrimMST[V, W: Numeric](graph: UndirectedWeightedGraph[V, W]) extends PrimMST[V, W] {

  protected val visited: mutable.Set[V] = mutable.Set[V]()
  private val pq = mutable.PriorityQueue.empty[WeightedEdge[V, W]](Ordering.by(weightOrder).reverse)
  private val mstWeight: W = main()

  protected def main(): W = {
    val connected = DFTConnected(graph)
    if (connected.isConnected) {
      var mstWeight: W = Numeric[W].zero
      val startingVertex: V = graph.vertices.head
      visit(startingVertex)
      while (pq.nonEmpty) {
        val edge = pq.dequeue()
        val WeightedEdge(vertex1, vertex2, weight) = edge
        breakable {
          if (visited.contains(vertex1) && visited.contains(vertex2)) {
            break
          } else {
            mstEdges += edge
            mstWeight += weight
            if (!visited.contains(vertex1)) visit(vertex1)
            if (!visited.contains(vertex2)) visit(vertex2)
          }
        }
      }
      mstWeight
    } else {
      throw GraphException(s"Graph $graph isn't connected.")
    }

  }


  /**
   * Visits the specified vertex and adds all its valid incident edges to the priority queue.
   *
   * @param vertex the vertex to visit
   */
  private def visit(vertex: V): Unit = {
    visited += vertex
    for (edge <- graph.incidentsFrom(vertex)) {
      val otherVertex = edge.vertex2
      if (!visited.contains(otherVertex)) {
        pq.enqueue(edge)
      }
    }

  }

  def getMstEdges: Set[WeightedEdge[V, W]] = Set[WeightedEdge[V, W]]() ++ mstEdges

  def totalWeight: W = mstWeight

  private def weightOrder(weightedEdge: WeightedEdge[V, W]): W = weightedEdge.weight

}
