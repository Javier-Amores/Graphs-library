import graph._
import graph.shortestPath.DirectedDijkstraShortestPath
object ShortestPathTest extends App {
  val dg = MapDirectedWeightedGraph[Int,Double]()
  for (i <- 0 to 7) {
    dg.addVertex(i)
  }
  dg.addEdge(0,2,0.26)
  dg.addEdge(0,4,0.38)
  dg.addEdge(2,7,0.34)
  dg.addEdge(4,5,0.35)
  dg.addEdge(7,3,0.37)
  dg.addEdge(5,1,0.32)
  dg.addEdge(3,6,0.52)
  dg.addEdge(5,4,0.35)
  dg.addEdge(4,7,0.37)
  dg.addEdge(5,7,0.28)
  dg.addEdge(7,5,0.28)
  dg.addEdge(1,3,0.29)
  dg.addEdge(6,2,0.4)
  dg.addEdge(6,0,0.58)
  dg.addEdge(6,4,0.93)

 val sp = DirectedDijkstraShortestPath[Int,Double](dg,0)
  for (i <- 0 to 7) {
    println(sp.distTo(i))
  }

  println(sp.pathTo(6))

  """
  val ord = Ordering[Double]

  val pq = IndexPriorityQueue[Double](10)( ord)
  pq.enqueue(8,0.55)
  pq.enqueue(4,0.38)
  println(pq.size())
  println(pq.dequeue())
  println(pq.size())
  pq.update(8,0.16)
  pq.enqueue(3,0.4)
  println(pq.size())
  println(pq.headIndex())
  println(pq.dequeue())
"""
}
