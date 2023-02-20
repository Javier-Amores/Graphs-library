import graph.DirectedEdge

object DirectedEdgeTest extends App{
  val directedEdge1 = DirectedEdge(1,2)
  val directedEdge2 = DirectedEdge(1,2)
  val directedEdge3 = DirectedEdge(2,1)
  val directedEdge4 = DirectedEdge(3,4)

  println(directedEdge1)

  assert(directedEdge1 == directedEdge1)
  assert(directedEdge1.hashCode() == directedEdge1.hashCode())
  assert(directedEdge1 == directedEdge2)
  assert(directedEdge1.hashCode() == directedEdge2.hashCode())
  assert(directedEdge2 == directedEdge1)
  assert(directedEdge2.hashCode() == directedEdge1.hashCode())

  assert(directedEdge1 != directedEdge3)
  assert(directedEdge3 != directedEdge4)
}
