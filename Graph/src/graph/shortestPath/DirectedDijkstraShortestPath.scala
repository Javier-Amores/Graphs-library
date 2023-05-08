package graph.shortestPath


import graph.{DirectedWeightedEdge, DirectedWeightedGraph, GraphException, IndexPriorityQueue, IsWeighted, WeightedEdge}


import scala.collection.mutable

case class DirectedDijkstraShortestPath[V, W: Numeric](graph: DirectedWeightedGraph[V, W], source: V)(implicit ord: Ordering[W]) extends DijkstraShortestPath[V, W, ({type E[X] = DirectedWeightedEdge[X, W]})#E] {

  private val edgeTo = Array.ofDim[DirectedWeightedEdge[V, W]](graph.order)
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

  private def main(): Unit = {
    val idToVertex: mutable.Map[Int, V] = for ((v, i) <- vertexToId) yield (i, v)

    distTo(vertexToId(source)) = null.asInstanceOf[W]
    pq.enqueue(vertexToId(source), null.asInstanceOf[W])
    while (pq.nonEmpty()) {
      val i = pq.dequeue()

      for (edge <- graph.incidentsFrom(idToVertex(i))) {
        val j = vertexToId(edge.destination)
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

  def pathTo[Edge[X] >: DirectedWeightedEdge[X, W]](vertex: V): Iterable[Edge[V]] = {
    if (!hasPathTo(vertex)) {
      throw GraphException(s"Vertex $vertex is not reachable from $source")
    }
    else {
      val path = new mutable.Stack[DirectedWeightedEdge[V, W]]()
      var edge = edgeTo(vertexToId(vertex))
      while (edge != null) {
        path.push(edge)
        edge = edgeTo(vertexToId(edge.source))
      }
      path.asInstanceOf[Iterable[Edge[V]]]
    }
  }

}
