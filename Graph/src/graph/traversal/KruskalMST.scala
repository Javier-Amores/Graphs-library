package graph.traversal

import graph.{UndirectedWeightedGraph, WeightedEdge}
import scala.collection.mutable
import util.control.Breaks._
import Numeric.Implicits._

case class KruskalMST[V,W:Numeric](graph: UndirectedWeightedGraph[V,W]) extends MinimumSpanningTree[V,W]{

  private val mstEdges = mutable.Set[WeightedEdge[V,W]]()
  private val mstWeight:W = main()

  private def main():W = {
    var mstWeight:W = null.asInstanceOf[W]
    val order = graph.order
    val edges:IterableOnce[WeightedEdge[V,W]] = { //to help infer types in compilation
      val aux = graph.edges
      aux
    }
    val pq = mutable.PriorityQueue[WeightedEdge[V,W]]()(Ordering.by(weightOrder).reverse)
    pq ++= edges
    val uf = new UnionFind(order)
    val IDVertexMap: mutable.Map[V,Int] = mutable.Map[V,Int]()
    var idNumber:Int = 0
    graph.vertices.foreach(vertex => {IDVertexMap(vertex) = idNumber
    idNumber += 1})

    while (pq.nonEmpty && mstEdges.size < order-1) {
      val edge = pq.dequeue()
      val WeightedEdge(vertex1, vertex2, weight) = edge
      val IDVertex1 = IDVertexMap(vertex1)
      val IDVertex2 = IDVertexMap(vertex2)
      breakable {
        if (uf.connected(IDVertex1,IDVertex2)) {
          break
        } else {
          mstWeight+=weight
          uf.union(IDVertex1,IDVertex2)
          mstEdges+=edge
        }
      }
    }
    mstWeight
  }

  private def weightOrder(weightedEdge: WeightedEdge[V,W]):W = weightedEdge.weight

  def getMstEdges: mutable.Set[WeightedEdge[V, W]] = mstEdges

  def totalWeight: W = mstWeight

}
