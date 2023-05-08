package graph.MST

import graph.{UndirectedWeightedGraph, UnionFind, WeightedEdge}

import scala.collection.mutable
import scala.math.Numeric.Implicits._
import scala.util.control.Breaks._

/**
 * A class that implements the Kruskal's algorithm to find the minimum spanning tree of an undirected weighted graph.
 *
 * @param graph the graph for which to find the minimum spanning tree
 * @tparam V the type of vertices in the graph
 * @tparam W the type of weights associated with the edges in the graph
 */
case class KruskalMST[V, W: Numeric](graph: UndirectedWeightedGraph[V, W]) extends MinimumSpanningTree[V, W] {

  private val mstEdges = mutable.Set[WeightedEdge[V, W]]()
  private val mstWeight: W = main()

  /**
   * Executes the Kruskal's algorithm to find the minimum spanning tree of the input graph.
   *
   * @return the total weight of the minimum spanning tree
   */
  private def main(): W = {
    var mstWeight: W = null.asInstanceOf[W]
    val order = graph.order
    val edges: IterableOnce[WeightedEdge[V, W]] = { //to help infer types in compilation
      val aux = graph.edges
      aux
    }
    val pq = mutable.PriorityQueue[WeightedEdge[V, W]]()(Ordering.by(weightOrder).reverse)
    pq ++= edges
    val uf = new UnionFind(order)
    val IDVertexMap: mutable.Map[V, Int] = mutable.Map[V, Int]()
    var idNumber: Int = 0
    graph.vertices.foreach(vertex => {
      IDVertexMap(vertex) = idNumber
      idNumber += 1
    })

    while (pq.nonEmpty && mstEdges.size < order - 1) {
      val edge = pq.dequeue()
      val WeightedEdge(vertex1, vertex2, weight) = edge
      val IDVertex1 = IDVertexMap(vertex1)
      val IDVertex2 = IDVertexMap(vertex2)
      breakable {
        if (uf.connected(IDVertex1, IDVertex2)) {
          break
        } else {
          mstWeight += weight
          uf.union(IDVertex1, IDVertex2)
          mstEdges += edge
        }
      }
    }
    mstWeight
  }

  private def weightOrder(weightedEdge: WeightedEdge[V, W]): W = weightedEdge.weight

  def getMstEdges: Set[WeightedEdge[V, W]] = Set[WeightedEdge[V, W]]() ++ mstEdges

  def totalWeight: W = mstWeight

}
