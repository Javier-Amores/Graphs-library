import graph.{DirectedWeightedEdge, MatrixDirectedWeightedGraphInt}

object MatrixDirectedWeightedGraphIntTest extends App {
  val g = MatrixDirectedWeightedGraphInt[Double](10)

  g.addVertex(0)
  g.addVertex(1)
  g.addVertex(2)
  g.addVertex(4)
  g.addVertex(7)
  g.addVertex(8)
  g.addVertex(9)

  g.addEdge(8, 0, 5.5)
  g.deleteVertex(8)

  assert(g.containsVertex(2))
  assert(!g.containsVertex(8))
  assert(!g.containsEdge(DirectedWeightedEdge(8, 0, 5.5)))

  println(g.vertices)
  println("g order is = " + g.order)

  //val edge = g.addEdge(0, 1)
  g.addEdge(0, 1, 0)
  g.addEdge(2, 1, 3.1415)
  //assert(g.containsEdge(edge))
  g.addEdge(DirectedWeightedEdge(4, 1, 7.9))
  g.addEdge(DirectedWeightedEdge(7, 9, 4.8))
  g.addEdge(DirectedWeightedEdge(4, 2, 2.1))
  g.addEdge(DirectedWeightedEdge(4, 7, 12.34))
  g.addEdge(4, 9, 0)
  g.addEdge(9, 4, 13.5)
  assert(g.containsEdge(DirectedWeightedEdge(4, 1, 7.9)))
  assert(g.containsEdge(4, 1))
  assert(g.containsEdge(4, 1, 7.9))
  g.deleteEdge(0, 1)
  g.deleteEdge(4, 9, 0)
  g.deleteEdge(DirectedWeightedEdge(9, 4, 13.5))


  println("Successors of 4 = " + g.successors(4))
  //println("Successors and weights of 4 = " + g.successorsAndWeights(4))
  println("predecessors of 4 = " + g.predecessors(4))
  println("predecessors of 9 = " + g.predecessors(9))
  //println("Predecessors and weights of 4 = " + g.predecessorsAndWeights(4))
  //println("predecessors and weight of 9 = " + g.predecessorsAndWeights(9))
  println("incidentsFrom of 4 = " + g.incidentsFrom(4))
  println("incidentsTo of 7 = " + g.incidentsTo(7))
  println("Indegree of 4 = " + g.indegree(4))
  println("Outdegree of 4 = " + g.outdegree(4))
  println("Degree of 4 = " + g.degree(4))
  println("Indegree of 9 = " + g.indegree(9))

  //g.deleteEdge(edge)
  println(g.edges)
  println("size of g = " + g.size)
  println("weight of edge (4,2) = " + g.weightOfEdge(4, 2))

}
