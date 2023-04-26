package graph.traversal

import graph._

import scala.collection.mutable

import util.control.Breaks._

case class PrimMST[V,W](graph: UndirectedWeightedGraph[V,W])(implicit ordering:Ordering[W]) extends MinimumSpanningTree[V,W]{

  private val mstEdges = mutable.Set[WeightedEdge[V,W]]()
  private val visited = mutable.Set[V]()
  private var mstWeight:W = null.asInstanceOf[W]
  private val pq:mutable.PriorityQueue[WeightedEdge[V,W]] = mutable.PriorityQueue[WeightedEdge[V,W]]()(Ordering.by(weightOrder).reverse)

  private def main():Unit = {
    val connected = DFTConnected(graph)
    if (connected.isConnected) {
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
           // mstWeight = mstWeight+weight
            if (!visited.contains(vertex1)) visit(vertex1)
            if (!visited.contains(vertex2)) visit(vertex2)
          }
        }
      }
    } else {
      throw GraphException(s"Graph $graph isn't connected.")
    }

  }

  main()

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
