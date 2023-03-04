import graph.{Edge, MapGraph}

object MapGraphTest extends App {
  val g = MapGraph[Int]()
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)
  g.addVertex(5)
  g.addVertex(6)

  g.addEdge(Edge(6, 5))
  g.deleteVertex(6)
  assert(!g.containsEdge(Edge(5, 6)))

  assert(g.containsVertex(4))
  assert(!g.containsVertex(6))


  g.addEdge(Edge(1, 2))
  g.addEdge(Edge(3, 1))
  g.addEdge(Edge(3, 4))
  g.addEdge(Edge(3, 5))
  g.addEdge(Edge(5, 4))
  val edge = g.addEdge(3, 2)
  assert(g.containsEdge(edge))
  g.deleteEdge(Edge(5, 3))

  assert(g.containsEdge(Edge(4, 3)))
  assert(!g.containsEdge(Edge(5, 3)))


  println(g.successors(3))
  println(g.vertices)
  println("order of g = " + g.order)
  println("degree of vertex 3 = " + g.degree(3))
  println(g.edges)
  assert(g.edges.contains(Edge(5, 4)))
  println(g.size)

}
