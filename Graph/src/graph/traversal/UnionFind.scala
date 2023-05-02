package graph.traversal

import scala.collection.mutable

/**
 * A class representing a union-find data structure with weighted quick-union with
 * path compression.
 *
 * @param maxComponent the maximum number of objects that can be represented by the data structure
 */
protected class UnionFind(maxComponent:Int){
  private val id: Array[Int] = Array.ofDim[Int](maxComponent)
  private val sz: Array[Int] = Array.ofDim[Int](maxComponent)
  private var count:Int = maxComponent

  for (i<- 0 until maxComponent) {
    id(i) = i
    sz(i) = 1
  }

  /**
   * Returns the number of components in the union-find data structure.
   * @return the number of components in the union-find data structure
   */
  def numberOfComponents():Int = count

  /**
   * Returns whether two elements are connected (i.e. belong to the same component) in the union-find data structure.
   * @param p one element to check
   * @param q  the other element to check
   * @return true if p and q are connected, false otherwise
   */
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

  /**
   * Returns the component id of p.
   * @param p the element to find the component id of
   * @return the component id of p
   */
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

  /**
   * Connects the components containing p and q.
   * @param p one element to connect
   * @param q  the other element to connect
   */
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
