package graph.shortestPath

import graph.priorityQueue.IndexPriorityQueue
import graph.{GraphException, IsWeightedEdge, WeightedGraph}

import scala.collection.mutable

/**
 * Represents a Dijkstra's shortest path algorithm for an undirected weighted graph.
 *
 * @param graph  The graph on which the algorithm is applied
 * @param source The source vertex from which to compute the shortest paths
 * @param ord    The ordering of weights used for comparisons
 * @tparam V The type of vertices in the graph
 * @tparam W The type of weights
 */
case class DijkstraShortestPath[V, W: Numeric](graph: WeightedGraph[V, W], source: V)(implicit ord: Ordering[W]) extends ShortestPath[V, W, ({type E[X] = IsWeightedEdge[X, W]})#E] {

  private val edgeTo: mutable.Map[V, IsWeightedEdge[V, W]] = mutable.Map[V, IsWeightedEdge[V, W]]()
  private val distTo = mutable.Map[V, W]()
  private val pq = IndexPriorityQueue[W](graph.order)(ord.reverse)


  /**
   * Executes the Dijkstra's shortest path algorithm.
   */
  private def main(): Unit = {
    val vertexToId: mutable.Map[V, Int] = {
      val m = mutable.Map[V, Int]()
      var idNumber: Int = 0
      graph.vertices.foreach(vertex => {
        m(vertex) = idNumber
        idNumber += 1
      })
      m
    }
    val idToVertex: mutable.Map[Int, V] = for ((v, i) <- vertexToId) yield (i, v)

    distTo(source) = Numeric[W].zero
    pq.enqueue(vertexToId(source), Numeric[W].zero)
    while (pq.nonEmpty()) {
      val vertexId = pq.dequeue()
      val vertex = idToVertex(vertexId)
      for (edge <- graph.incidentsFrom(vertex)) {
        val successor = edge.vertex2
        val successorId = vertexToId(successor)
        distTo.get(successor) match {
          case None => distTo(successor) = Numeric[W].plus(distTo(vertex), edge.weight)
            edgeTo(successor) = edge
            if (pq.contains(successorId)) {
              pq.update(successorId, distTo(successor))
            }
            else {
              pq.enqueue(successorId, distTo(successor))
            }
          case Some(successorDistance) => distTo.get(vertex) match {
            case Some(vertexDistance) if ord.compare(successorDistance, Numeric[W].plus(vertexDistance, edge.weight)) > 0 => distTo(successor) = Numeric[W].plus(vertexDistance, edge.weight)
              edgeTo(successor) = edge
              if (pq.contains(successorId)) {
                pq.update(successorId, successorDistance)
              }
              else {
                pq.enqueue(successorId, successorDistance)
              }
            case _ =>
          }
        }

      }
    }
  }

  main()

  def distTo(vertex: V): Option[W] = distTo.get(vertex)

  def hasPathTo(vertex: V): Boolean = distTo.get(vertex) match {
    case None => false
    case _ => true
  }

  def pathTo[Edge[X] >: IsWeightedEdge[X, W]](vertex: V): Iterable[Edge[V]] = {
    if (!hasPathTo(vertex)) {
      throw GraphException(s"Vertex $vertex is not reachable from $source")
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
