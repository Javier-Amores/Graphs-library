package graph.shortestPath

import graph.{GraphException, DirectedWeightedEdge, DirectedWeightedGraph, IndexPriorityQueue, MapDirectedWeightedGraph, UndirectedWeightedGraph, WeightedEdge}

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
case class UndirectedDijkstraShortestPath[V, W: Numeric](graph: UndirectedWeightedGraph[V, W], source: V)(implicit ord: Ordering[W]) extends DijkstraShortestPath[V, W, ({type E[X] = WeightedEdge[X, W]})#E] {

  private val edgeTo = Array.ofDim[WeightedEdge[V, W]](graph.order)
  private val distTo = mutable.Map[Int, W]()
  private val pq = IndexPriorityQueue[W](graph.order)(ord.reverse)

  private val vertexToId: mutable.Map[V, Int] = {
    val m = mutable.Map[V, Int]()
    var idNumber: Int = 0
    graph.vertices.foreach(vertex => {
      m(vertex) = idNumber
      idNumber += 1
    })
    m
  }

  /**
   * Executes the Dijkstra's shortest path algorithm.
   */
  private def main(): Unit = {
    val idToVertex: mutable.Map[Int, V] = for ((v, i) <- vertexToId) yield (i, v)

    distTo(vertexToId(source)) = null.asInstanceOf[W]
    pq.enqueue(vertexToId(source), null.asInstanceOf[W])
    while (pq.nonEmpty()) {
      val i = pq.dequeue()

      for (edge <- graph.incidentsFrom(idToVertex(i))) {
        val j = vertexToId(edge.vertex2)
        distTo.get(j) match {
          case None => distTo(j) = implicitly[Numeric[W]].plus(distTo(i), edge.weight)
            edgeTo(j) = edge
            if (pq.contains(j)) {
              pq.update(j, distTo(j))
            }
            else {
              pq.enqueue(j, distTo(j))
            }
          case Some(jDistance) => distTo.get(i) match {
            case Some(iDistance) if ord.compare(jDistance, implicitly[Numeric[W]].plus(iDistance, edge.weight)) > 0 => distTo(j) = implicitly[Numeric[W]].plus(iDistance, edge.weight)
              edgeTo(j) = edge
              if (pq.contains(j)) {
                pq.update(j, jDistance)
              }
              else {
                pq.enqueue(j, jDistance)
              }
            case _ =>
          }
        }

      }
    }
  }

  main()

  def distTo(vertex: V): Option[W] = distTo.get(vertexToId(vertex)) match {
    case None => None
    case Some(distance) => Some(distance)
  }

  def hasPathTo(vertex: V): Boolean = distTo.get(vertexToId(vertex)) match {
    case None => false
    case _ => true
  }

  def pathTo[Edge[X] >: WeightedEdge[X, W]](vertex: V): Iterable[Edge[V]] = {
    if (!hasPathTo(vertex)) {
      throw GraphException(s"Vertex $vertex is not reachable from $source")
    }
    else {
      val path = new mutable.Stack[WeightedEdge[V, W]]()
      var edge = edgeTo(vertexToId(vertex))
      while (edge != null) {
        path.push(edge)
        edge = edgeTo(vertexToId(edge.vertex1))
      }
      path.asInstanceOf[Iterable[Edge[V]]]
    }
  }

}
