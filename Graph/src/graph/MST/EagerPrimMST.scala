package graph.MST

import graph.traversal.DFTConnected
import graph.{GraphException, IndexPriorityQueue, UndirectedWeightedGraph, WeightedEdge}

import scala.collection.mutable
import scala.util.control.Breaks._
import scala.math.Numeric.Implicits._


case class EagerPrimMST[V, W: Numeric](graph: UndirectedWeightedGraph[V, W])(implicit ord: Ordering[W]) extends PrimMST[V, W] {
  private val visited: Array[Boolean] = Array.fill(graph.order)(false)
  private val pq = IndexPriorityQueue[W](graph.order)(ord)
  private val edgeTo: Array[WeightedEdge[V, W]] = Array.ofDim[WeightedEdge[V, W]](graph.order)
  private val mstWeight: W = main()


  protected def main(): W = {
    val connected = DFTConnected(graph)
    if (connected.isConnected) {
      val vertexToId: mutable.Map[V, Int] = mutable.Map[V, Int]()
      var idNumber: Int = 0
      graph.vertices.foreach(vertex => {
        vertexToId(vertex) = idNumber
        idNumber += 1
      })
      val idToVertex: mutable.Map[Int, V] = for ((v, i) <- vertexToId) yield (i, v)
      val vertexDistance: mutable.Map[Int, W] = mutable.Map[Int, W]()
      vertexDistance(0) = null.asInstanceOf[W]
      pq.enqueue(0,vertexDistance(0))
      while (pq.nonEmpty()) {
        val vertexId = pq.dequeue()
        visited(vertexId) = true
        for (edge <- graph.incidentsFrom(idToVertex(vertexId))) {
          val adjacentVertexId: Int = vertexToId(edge.vertex2)
          breakable {
            if (visited(adjacentVertexId)) {
              break
            } else {
              vertexDistance.get(adjacentVertexId) match {
                case None =>
                  edgeTo(adjacentVertexId) = edge
                  vertexDistance(adjacentVertexId) = edge.weight
                  if (pq.contains(adjacentVertexId)) {
                    pq.update(adjacentVertexId, vertexDistance(adjacentVertexId))
                  }
                  else {
                    pq.enqueue(adjacentVertexId, vertexDistance(adjacentVertexId))
                  }

                case  Some(distance) if ord.compare(edge.weight, distance)<0 =>
                  edgeTo(adjacentVertexId) = edge
                  vertexDistance(adjacentVertexId) = edge.weight
                  if (pq.contains(adjacentVertexId)) {
                    pq.update(adjacentVertexId, vertexDistance(adjacentVertexId))
                  }
                  else {
                    pq.enqueue(adjacentVertexId, vertexDistance(adjacentVertexId))
                  }

                case _ =>
              }
            }
          }
        }
      }
      var totalWeight:W = null.asInstanceOf[W]
      vertexDistance.foreach {case (_,cost) => totalWeight+=cost;case _ =>}
      totalWeight
    } else {
      throw GraphException(s"Graph $graph isn't connected.")
    }

  }

  def getMstEdges: Set[WeightedEdge[V, W]] = edgeTo.toSet.diff(Set(null))

  def totalWeight: W = mstWeight


}
