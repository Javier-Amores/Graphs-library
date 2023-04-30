package graph.traversal

import scala.collection.mutable

protected class UnionFind(maxComponent:Int){
  private val id: Array[Int] = Array.ofDim[Int](maxComponent)
  private val sz: Array[Int] = Array.ofDim[Int](maxComponent)
  private var count:Int = maxComponent

  for (i<- 0 until maxComponent) {
    id(i) = i
    sz(i) = 1
  }

  def numberOfComponents():Int = count
  def connected(p: Int, q: Int): Boolean = find(p) == find(q)

  //quick-find implementation
  """
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
  """

  //weighted quick-union with path compression implementation
  def find(p :Int):Int = {
    var root:Int = p
    val nodes = mutable.Set[Int](p)
    while (root!=id(root)) {
      root = id(root)
      nodes += root
    }
    val nodesIt = nodes.iterator
    while(nodesIt.hasNext) {
      val nextNode = nodesIt.next()
      if (nextNode!=root) {id(nextNode)=root}
    }
    root
  }
  def union(p:Int, q:Int):Unit = {
    val pRoot:Int = find(p)
    val qRoot:Int = find(q)
    if (pRoot==qRoot) {}
    else if (sz(pRoot)<sz(qRoot)) {
      id(pRoot) = qRoot
      sz(qRoot)+= sz(pRoot)
    }
    else {
      id(qRoot) = pRoot
      sz(pRoot) += sz(qRoot)
    }
    count-=1
  }


}
