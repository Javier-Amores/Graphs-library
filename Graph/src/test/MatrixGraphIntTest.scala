import graph.{Edge, MatrixGraphInt}

object MatrixGraphIntTest extends App {
  val g = MatrixGraphInt(10)
  g.addVertex(0)
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(4)
  g.addVertex(7)
  g.addVertex(8)
  g.addVertex(9)

  g.addEdge(8,0)
  g.deleteVertex(8)

  assert(g.containsVertex(2))
  assert(!g.containsVertex(8))
  assert(!g.containsEdge(Edge(0,8)))


}
