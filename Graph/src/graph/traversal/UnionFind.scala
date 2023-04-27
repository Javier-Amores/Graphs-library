package graph.traversal

protected class UnionFind(maxComponent:Int){
  private val id: Array[Int] = Array.ofDim[Int](maxComponent)
  private var count:Int = maxComponent

  for (i<- 0 until maxComponent) {
    id(i) = i
  }

  def numberOfComponents():Int = count
  def connected(p: Int, q: Int): Boolean = find(p) == find(q)

  //quick-find implementation
  def find(p :Int):Int = id(p) //O(1)
  def union(p:Int, q:Int):Unit = { //O(N)
    val pID = find(p)
    val qID = find(q)
    if (pID == qID) {
    } else {
      for (i <- 0 until maxComponent) {
        if (id(i)==pID) {id(i)=qID}
      }
      count -= 1
    }
  }

}
