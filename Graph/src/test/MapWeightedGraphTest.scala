import graph.{MapWeightedGraph, WeightedEdge}

object MapWeightedGraphTest extends App {
  val g = MapWeightedGraph[String, Int]()

  g.addVertex("A")
  g.addVertex("B")
  g.addVertex("C")
  g.addVertex("D")
  g.addVertex("E")
  g.addVertex("F")
  g.addVertex("G")

  g.addEdge(WeightedEdge("E", "F", 2))
  g.deleteVertex("F")
  assert(!g.containsVertex("F"))
  assert(!g.containsEdge(WeightedEdge("F", "E", 2)))

  assert(g.containsVertex("A"))
  assert(g.containsVertex("B"))

  println(g.vertices)
  println("order of g = " + g.order)

  g.addEdge(WeightedEdge("A", "B", 1))
  g.addEdge(WeightedEdge("A", "C", 3))
  g.addEdge(WeightedEdge("D", "A", 4))
  g.addEdge(WeightedEdge("G", "B", 0))
  g.addEdge(WeightedEdge("C", "D", 2))
  g.addEdge("E", "G", 0)
  //val nullWeightedEdge = g.addEdge("E", "G")
  //assert(nullWeightedEdge != WeightedEdge("E", "G", 0))
  println(g.containsEdge(WeightedEdge("E", "G", 0)))
  g.addEdge("A", "G", 99)

  println("adjacent of A = " + g.adjacents("A"))
  println("successors of A = " + g.successors("A"))
  println("predecessors of A = " + g.predecessors("A"))
  //("successors and weights of A = " + g.successorsAndWeights("A"))
  println(g.incidents("A"))
  println(g.incidentsFrom("A"))
  println(g.incidentsTo("A"))
  println("degree of A = " + g.degree("A"))
  println("degree of E = " + g.degree("E"))

  assert(g.containsEdge(WeightedEdge("B", "A", 1)))
  assert(g.containsEdge("C", "A"))
  assert(g.containsEdge("C", "A", 3))

  g.deleteEdge(WeightedEdge("B", "G", 0))
  println(g.weightOfEdge("A", "C"))
  g.deleteEdge("A", "C")
  g.deleteEdge("A", "D", 4)

  println("size of g = " + g.size)
  println(g.edges)

}
