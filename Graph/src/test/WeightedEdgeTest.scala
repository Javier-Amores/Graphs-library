import graph.WeightedEdge

object WeightedEdgeTest extends App{
  val weightedEdge1 = WeightedEdge("A","B",1)
  val weightedEdge2 = WeightedEdge("A","B",1)
  val weightedEdge3 = WeightedEdge("B","A",1)
  val weightedEdge4 = WeightedEdge("B","A",-1)
  val weightedEdge5 = WeightedEdge("C","D",1)
  val weightedEdge6 = WeightedEdge("C","D",null)

  println(weightedEdge1)
  println(weightedEdge6)

  assert(weightedEdge1 == weightedEdge1)
  assert(weightedEdge1.hashCode() == weightedEdge1.hashCode())
  assert(weightedEdge1 == weightedEdge2)
  assert(weightedEdge1.hashCode() == weightedEdge2.hashCode())
  assert(weightedEdge2 == weightedEdge1)
  assert(weightedEdge2.hashCode() == weightedEdge1.hashCode())
  assert(weightedEdge2 == weightedEdge3)
  assert(weightedEdge2.hashCode() == weightedEdge3.hashCode())

  assert(weightedEdge3 != weightedEdge4)
  assert(weightedEdge1 != weightedEdge5)
  assert(weightedEdge4 != weightedEdge5)
  assert(weightedEdge5 != weightedEdge6)
}
