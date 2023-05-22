package graph.MST

import graph.priorityQueue.IndexPriorityQueue
import graph.traversal.DFTConnected
import graph.{GraphException, UndirectedWeightedGraph, WeightedEdge}

import scala.collection.{immutable, mutable}
import scala.util.control.Breaks._
import scala.math.Numeric.Implicits._


case class EagerPrimMST[V, W: Numeric](graph: UndirectedWeightedGraph[V, W])(implicit ord: Ordering[W]) extends PrimMST[V, W] {
  private val edgeTo : mutable.Map[V,WeightedEdge[V, W]] = mutable.Map[V,WeightedEdge[V, W]]()
  private val mstWeight: W = main()

  protected def main(): W = {
    val pq = IndexPriorityQueue[W](graph.order)(ord)
    val visited: mutable.Set[V] = mutable.Set[V]()
    val connected = DFTConnected(graph)
    if (connected.isConnected) {
      val vertexToId: mutable.Map[V, Int] = mutable.Map[V, Int]()
      var idNumber: Int = 0
      graph.vertices.foreach(vertex => {
        vertexToId(vertex) = idNumber
        idNumber += 1
      })
      val idToVertex: mutable.Map[Int, V] = for ((v, i) <- vertexToId) yield (i, v)
      val vertexDistance: mutable.Map[V, W] = mutable.Map[V, W]()
      vertexDistance(idToVertex(0)) = Numeric[W].zero
      pq.enqueue(0, Numeric[W].zero)
      while (pq.nonEmpty()) {
        val vertexId = pq.dequeue()
        visited += idToVertex(vertexId)
        for (edge <- graph.incidentsFrom(idToVertex(vertexId))) {
          val adjacentVertex: V = edge.vertex2
          breakable {
            if (visited.contains(adjacentVertex)) {
              break
            } else {
              vertexDistance.get(adjacentVertex) match {
                case None =>
                  edgeTo(adjacentVertex) = edge
                  vertexDistance(adjacentVertex) = edge.weight
                  if (pq.contains(vertexToId(adjacentVertex))) {
                    pq.update(vertexToId(adjacentVertex), vertexDistance(adjacentVertex))
                  }
                  else {
                    pq.enqueue(vertexToId(adjacentVertex), vertexDistance(adjacentVertex))
                  }

                case Some(distance) if ord.compare(edge.weight, distance) < 0 =>
                  edgeTo(adjacentVertex) = edge
                  vertexDistance(adjacentVertex) = edge.weight
                  if (pq.contains(vertexToId(adjacentVertex))) {
                    pq.update(vertexToId(adjacentVertex), vertexDistance(adjacentVertex))
                  }
                  else {
                    pq.enqueue(vertexToId(adjacentVertex), vertexDistance(adjacentVertex))
                  }

                case _ =>
              }
            }
          }
        }
      }
      var totalWeight: W = Numeric[W].zero
      vertexDistance.foreach { case (_, cost) => totalWeight += cost; case _ => }
      totalWeight
    } else {
      throw GraphException(s"Graph $graph isn't connected.")
    }

  }

  def getMstEdges: immutable.Set[WeightedEdge[V, W]] = edgeTo.values.toSet

  def totalWeight: W = mstWeight


}
