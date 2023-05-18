
import graph._
import graph.traversal.DirectedCycleFinder

object DirectedCycleFinderTest extends App {
  val g = MatrixDirectedWeightedGraphInt[Int](4)
  g.addVertex(0)
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addEdge(0, 1, 0)
  g.addEdge(1, 2, 0)
  g.addEdge(2, 3, 0)

  var cf = DirectedCycleFinder(g)
  println(cf.hasCycle)
  println(cf.cycle())
  g.addEdge(3, 1, 0)
  cf = DirectedCycleFinder(g)
  println(cf.hasCycle)
  println(cf.cycle())
}
