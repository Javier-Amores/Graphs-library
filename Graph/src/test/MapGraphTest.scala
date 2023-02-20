import graph.MapGraph

object MapGraphTest extends App{
  val g = MapGraph[Int]()
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)
  g.addVertex(5)
  g.addVertex(6)
  g.deleteVertex(6)
  println(g.containsVertex(6))
  println(g.containsVertex(5))


}
