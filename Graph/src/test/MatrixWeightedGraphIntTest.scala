import graph.{MatrixWeightedGraphInt, WeightedEdge}

object MatrixWeightedGraphIntTest extends App {

  val g = MatrixWeightedGraphInt[Double](10)

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
  assert(!g.containsEdge(WeightedEdge(8, 0, 5.5)))

  println(g.vertices)
  println("g order is = " + g.order)

  //val edge = g.addEdge(0, 1)
  g.addEdge(2, 1, 3.1415)
  //assert(g.containsEdge(edge))
  g.addEdge(WeightedEdge(4, 1, 7.9))
  g.addEdge(WeightedEdge(7, 9, 4.8))
  g.addEdge(WeightedEdge(4, 2, 2.1))
  g.addEdge(WeightedEdge(4, 7, 12.34))
  g.addEdge(WeightedEdge(2, 1, 1.3))
  g.addEdge(4, 9, 0)
  g.addEdge(3, 4, 19)
  //g.addEdge(4, 9)
  g.deleteEdge(4, 7)
  g.deleteEdge(1, 2, 1.3)
  g.deleteEdge(WeightedEdge(3, 4, 19))


  assert(g.containsEdge(4, 1))
  assert(g.containsEdge(4, 1, 7.9))
  assert(g.containsEdge(WeightedEdge(4, 2, 2.1)))

  println("adjacent of 4 = " + g.adjacents(4))
  println("Successors of 4 = " + g.successors(4))
  println("predecessors of 4 = " + g.predecessors(4))
  //println("Successors and weights of 4 = " + g.successorsAndWeights(4))
  println(g.incidents(4))
  println(g.incidentsFrom(4))
  println(g.incidentsTo(4))
  println("Degree of 4 = " + g.degree(4))

  //g.deleteEdge(edge)
  println(g.edges)
  println("size of g = " + g.size)
  println(g.weightOfEdge(1, 4))

}
