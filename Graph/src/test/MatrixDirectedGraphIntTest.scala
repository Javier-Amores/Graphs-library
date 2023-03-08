import graph.{DirectedEdge, MatrixDirectedGraphInt}

object MatrixDirectedGraphIntTest extends App {

  val g = MatrixDirectedGraphInt(10)

  g.addVertex(0)
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(4)
  g.addVertex(7)
  g.addVertex(8)
  g.addVertex(9)

  g.addEdge(8, 0)
  g.deleteVertex(8)

  assert(g.containsVertex(2))
  assert(!g.containsVertex(8))
  assert(!g.containsEdge(DirectedEdge(8, 0)))

  println(g.vertices)
  println("g order is = " + g.order)

  val edge = g.addEdge(0, 1)
  assert(g.containsEdge(edge))
  g.addEdge(DirectedEdge(4, 1))
  g.addEdge(DirectedEdge(7, 9))
  g.addEdge(DirectedEdge(4, 2))
  g.addEdge(DirectedEdge(4, 7))

  println("Successors of 4 = " + g.successors(4))
  println("predecessors of 4 = " + g.predecessors(4))
  println("predecessors of 9 = " + g.predecessors(9))
  println("Indegree of 4 = " + g.indegree(4))
  println("Outdegree of 4 = " + g.outdegree(4))

  g.deleteEdge(edge)
  println(g.edges)
  println("size of g = " + g.size)
}
