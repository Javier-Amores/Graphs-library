import graph.{DirectedEdge, MapDirectedGraph}

object MapDirectedGraphTest extends App {
  val g = MapDirectedGraph[Int]()
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(3)
  g.addVertex(4)
  g.addVertex(5)
  g.addVertex(6)
  g.addVertex(7)

  g.addEdge(DirectedEdge(5,6))
  g.addEdge(DirectedEdge(6,4))
  g.deleteVertex(6)


  assert(g.containsVertex(5))
  assert(!g.containsVertex(6))

  println(g.vertices)
  println("graph order = " + g.order)
  g.addEdge(DirectedEdge(1,2))
  g.addEdge(DirectedEdge(1,3))
  g.addEdge(DirectedEdge(3,2))
  g.addEdge(DirectedEdge(4,1))
  g.addEdge(DirectedEdge(4,5))
  val directedEdge = g.addEdge(5,4)
  assert(g.containsEdge(directedEdge))

  println("successors of 1 = " + g.successors(1))
  println("successors of 4 = " + g.successors(4))
  println("successors of 5 = " + g.successors(5))
  println("successors of 7 = " + g.successors(7))

  println("predecessors of 1 = " + g.predecessors(1))
  println("predecessors of 3 = " + g.predecessors(3))
  println("predecessors of 7 = " + g.predecessors(7))

  println("degree of 1 = "+g.degree(1))
  println("indegree of 1 = "+g.indegree(1))
  println("outdegree of 1 = "+g.outdegree(1))

  g.deleteEdge(DirectedEdge(3,2))
  assert(!g.containsEdge(DirectedEdge(3,2)))
  assert(g.containsEdge(DirectedEdge(4,5)))
  println(g.edges)
  println(g.size)



}