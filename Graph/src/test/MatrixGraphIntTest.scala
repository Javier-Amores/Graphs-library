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

  g.addEdge(8, 0)
  g.deleteVertex(8)

  assert(g.containsVertex(2))
  assert(!g.containsVertex(8))
  assert(!g.containsEdge(Edge(0, 8)))

  println(g.vertices)
  println("g order is = " + g.order)

  //val edge = g.addEdge(0, 1)
  //assert(g.containsEdge(edge))
  g.addEdge(Edge(4, 1))
  g.addEdge(Edge(7, 9))
  g.addEdge(Edge(4, 2))
  g.addEdge(Edge(4, 7))
  g.addEdge(0, 1)
  assert(g.containsEdge(0, 1))
  assert(g.containsEdge(Edge(0, 1)))

  println("Adjacent of 4 =" + g.adjacents(4))
  println("Successors of 4 = " + g.successors(4))
  println("Predecessors of 4 = " + g.predecessors(4))
  println("Degree of 4 = " + g.degree(4))

  println(g.incidentsFrom(4))
  println(g.incidentsTo(4))
  println(g.incidents(4))

  g.deleteEdge(0, 1)
  g.deleteEdge(Edge(7, 9))
  println(g.edges)
  println("size of g = " + g.size)

}
