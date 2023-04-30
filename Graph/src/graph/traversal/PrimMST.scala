package graph.traversal

import graph._
import scala.collection.mutable
import util.control.Breaks._
import Numeric.Implicits._

case class PrimMST[V,W:Numeric](graph: UndirectedWeightedGraph[V,W]) extends MinimumSpanningTree[V,W]{

  private val mstEdges = mutable.Set[WeightedEdge[V,W]]()
  private val visited = mutable.Set[V]()
  private val pq = mutable.PriorityQueue.empty[WeightedEdge[V,W]](Ordering.by(weightOrder).reverse)
  private val mstWeight:W = main()

  private def main():W = {
    val connected = DFTConnected(graph)
    if (connected.isConnected) {
      var mstWeight:W = null.asInstanceOf[W]
      val startingVertex:V = graph.vertices.head
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


  private def visit(vertex: V): Unit = {
    visited += vertex
    for (edge <- graph.incidentsFrom(vertex)) {
      val otherVertex = edge.vertex2
      if (!visited.contains(otherVertex)) {
        pq.enqueue(edge)
      }
    }

  }

  private def weightOrder(weightedEdge: WeightedEdge[V,W]):W = weightedEdge.weight

  def getMstEdges:mutable.Set[WeightedEdge[V,W]] = mstEdges

  def totalWeight:W = mstWeight

}
