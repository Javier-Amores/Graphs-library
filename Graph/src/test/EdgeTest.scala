import graph.Edge

object EdgeTest extends App {
  val edge1 = Edge[Int](1, 2)
  val edge2 = Edge[Int](1, 2)
  val edge3 = Edge[Int](2, 1)
  val edge4 = Edge[Int](3, 4)
  val edge5 = Edge[Int](4, 5)

  println(edge1)

  assert(edge1 == edge1)
  assert(edge1.hashCode() == edge1.hashCode())
  assert(edge1 == edge2)
  assert(edge1.hashCode() == edge2.hashCode())
  assert(edge2 == edge1)
  assert(edge2.hashCode() == edge1.hashCode())
  assert(edge1 == edge3)
  assert(edge1.hashCode() == edge3.hashCode())

  assert(edge1 != edge4)
  assert(edge4 != edge5)


}