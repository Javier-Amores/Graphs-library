import graph._
import graph.traversal.PrimMST

object MSTTest extends App{

  val g = MapWeightedGraph[Int, Double]()
  for (i <- 0 to 7) {
    g.addVertex(i)
  }
  g.addEdge(0,7,0.16)
  g.addEdge(0,2,0.26)
  g.addEdge(0,4,0.38)
  g.addEdge(0,6,0.58)
  g.addEdge(4,6,0.93)
  g.addEdge(4,7,0.37)
  g.addEdge(2,7,0.34)
  g.addEdge(2,6,0.40)
  g.addEdge(4,5,0.35)
  g.addEdge(5,7,0.28)
  g.addEdge(5,1,0.32)
  g.addEdge(1,2,0.36)
  g.addEdge(1,7,0.19)
  g.addEdge(1,3,0.29)
  g.addEdge(3,2,0.17)
  g.addEdge(3,6,0.52)

  val mst = PrimMST(g)
  println(mst.getMstEdges)
}
