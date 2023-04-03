import graph._
import graph.traversal.{DepthFirstTraversal, BreadthFirstTraversal}

object TraversalTest extends App {
  val g = MapGraph[String]()

  g.addVertex("A")
  g.addVertex("B")
  g.addVertex("C")
  g.addVertex("D")
  g.addVertex("E")
  g.addVertex("F")
  g.addVertex("G")
  g.addVertex("H")
  g.addVertex("I")

  g.addEdge("A", "C")
  g.addEdge("A", "D")
  g.addEdge("A", "H")
  g.addEdge("C", "G")
  g.addEdge("H", "B")
  g.addEdge("G", "E")
  g.addEdge("H", "E")
  g.addEdge("F", "E")
  g.addEdge("F", "D")
  g.addEdge("D", "E")

  val t = new DepthFirstTraversal(g, "A")
  val b = new BreadthFirstTraversal(g, "A")


  println(t.isReachable("B"))
  println(t.isReachable("I"))
  println(b.isReachable("G"))
  println(t.pathTo("B"))
  println(t.pathTo("I"))
  println(b.pathTo("F"))

  val dg = MapDirectedWeightedGraph[String,Int]()
  dg.addVertex("A")
  dg.addVertex("B")
  dg.addVertex("C")
  dg.addVertex("D")
  dg.addVertex("E")
  dg.addVertex("F")
  dg.addVertex("G")
  dg.addVertex("H")
  dg.addVertex("I")

  dg.addEdge("A", "C",0)
  dg.addEdge("A", "D",0)
  dg.addEdge("A", "H",0)
  dg.addEdge("C", "G",0)
  dg.addEdge("H", "B",0)
  dg.addEdge("G", "E",0)
  dg.addEdge("H", "E",0)
  dg.addEdge("F", "E",0)
  dg.addEdge("F", "D",0)
  dg.addEdge("D", "E",0)

  println()
  val dt = new DepthFirstTraversal(dg, "A")
  val db = new BreadthFirstTraversal(dg, "A")


  println(dt.isReachable("B"))
  println(dt.isReachable("I"))
  println(db.isReachable("G"))
  println(dt.pathTo("B"))
  println(dt.pathTo("I"))
  println(db.pathTo("F"))



}
