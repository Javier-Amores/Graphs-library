import graph.{DirectedWeightedEdge, MapDirectedWeightedGraph}

object MapDirectedWeightedGraphTest extends App {
  val g = MapDirectedWeightedGraph[String, Int]()

  g.addVertex("A")
  g.addVertex("B")
  g.addVertex("C")
  g.addVertex("D")
  g.addVertex("E")
  g.addVertex("F")
  g.addVertex("G")

  g.addEdge(DirectedWeightedEdge("E", "G", 10))
  g.addEdge(DirectedWeightedEdge("G", "E", 0))
  g.addEdge(DirectedWeightedEdge("G", "F", 100))
  g.addEdge(DirectedWeightedEdge("F", "G", 20))


  g.deleteVertex("G")

  assert(g.containsVertex("A"))
  assert(!g.containsVertex("G"))
  assert(!g.containsEdge(DirectedWeightedEdge("E", "G", 10)))

  println(g.vertices)
  println("order of g = " + g.order)

  g.addEdge(DirectedWeightedEdge("A", "B", 1))
  g.addEdge(DirectedWeightedEdge("A", "C", 3))
  g.addEdge(DirectedWeightedEdge("A", "D", 2))
  g.addEdge(DirectedWeightedEdge("B", "C", 7))
  g.addEdge(DirectedWeightedEdge("C", "A", 72))
  g.addEdge(DirectedWeightedEdge("D", "F", 7))
  g.addEdge(DirectedWeightedEdge("E", "A", 54))
  g.addEdge(DirectedWeightedEdge("E", "B", 4))
  g.addEdge(DirectedWeightedEdge("E", "F", 5))
  g.addEdge(DirectedWeightedEdge("F", "E", 5))
  g.addEdge("C", "D", 10)
  g.addEdge("B", "A", 0)
  g.addEdge("F", "D", 12)

  g.deleteEdge(DirectedWeightedEdge("C", "A", 72))
  g.deleteEdge("E", "F")
  g.deleteEdge("B", "C", 7)

  assert(g.containsEdge(DirectedWeightedEdge("F", "E", 5)))
  assert(!g.containsEdge("F", "F"))
  assert(!g.containsEdge(DirectedWeightedEdge("C", "A", 72)))
  assert(g.containsEdge("C", "D"))
  assert(g.containsEdge("E", "A", 54))
  assert(!g.containsEdge("E", "A", 4))

  println("successors of A = " + g.successors("A"))
  println("predecessors of A = " + g.predecessors("A"))
  //println("successors and weights of A = " + g.successorsAndWeights("A"))
  //println("predecessors and weights of A = " + g.predecessorsAndWeights("A"))
  println("incidentsFrom of A = " + g.incidentsFrom("A"))
  println("incidentsTo of A = " + g.incidentsTo("A"))


  println(g.edges)
  println("size of g = " + g.size)
  println(g.indegree("A"))
  println(g.outdegree("A"))
  println(g.degree("A"))
  println("weight of edge (F,D) = " + g.weightOfEdge("F", "D").get)


}
