import graph.DirectedWeightedEdge

object DirectedWeightedEdgeTest extends App{
  val directedWeightedEdge1 = DirectedWeightedEdge("A","B",3)
  val directedWeightedEdge2 = DirectedWeightedEdge("A","B",3)
  val directedWeightedEdge3 = DirectedWeightedEdge("B","A",3)
  val directedWeightedEdge4 = DirectedWeightedEdge("B","A",5)
  val directedWeightedEdge5 = DirectedWeightedEdge(1,2,"A")
  val directedWeightedEdge6 = DirectedWeightedEdge(0,1,null)
  val directedWeightedEdge7 = DirectedWeightedEdge(0,1,null)

  println(directedWeightedEdge1)

  assert(directedWeightedEdge1 == directedWeightedEdge1)
  assert(directedWeightedEdge1.hashCode() == directedWeightedEdge1.hashCode())
  assert(directedWeightedEdge1 == directedWeightedEdge2)
  assert(directedWeightedEdge1.hashCode() == directedWeightedEdge2.hashCode())
  assert(directedWeightedEdge2 == directedWeightedEdge1)
  assert(directedWeightedEdge2.hashCode() == directedWeightedEdge1.hashCode())
  assert(directedWeightedEdge6 == directedWeightedEdge7)
  assert(directedWeightedEdge6.hashCode() == directedWeightedEdge7.hashCode())

  assert(directedWeightedEdge1 != directedWeightedEdge3)
  assert(directedWeightedEdge4 != directedWeightedEdge3)
  assert(directedWeightedEdge4 != directedWeightedEdge5)
}
