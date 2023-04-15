package graph.traversal

import graph._

import scala.collection.mutable


abstract class ConnectedComponent[V](graph: UndirectedGraph[V]) {

  private val vertexToNumberOfComponent: mutable.Map[V, Int] = mutable.Map[V, Int]()
  private val componentNumberToSet: mutable.Map[Int, mutable.Set[V]] = mutable.Map[Int, mutable.Set[V]]()

  protected def traversal(starVertex:V): FirstTraversal[V]

  private def funtion():Unit ={
    var vertices = graph.vertices.iterator
    var counter: Int = 0
    while (vertices.hasNext) {
      val vertex = vertices.next()
      componentNumberToSet(counter) = mutable.Set[V]()
      val traversalOfVertex = traversal(vertex)
      traversalOfVertex.getSpanningTree.foreach {case (successor,_) => vertexToNumberOfComponent(successor) = counter
        componentNumberToSet(counter) += successor
      vertices = vertices.filterNot(node => node==successor)}

      counter+=1
    }
  }

  funtion()


  /**
   * Get the set of vertices in the same connected component as the specified vertex.
   *
   * @param vertex the vertex to find the connected component of
   * @return a set of vertices in the same connected component
   */
  def componentOf(vertex: V): Set[V] = Set.empty++ componentNumberToSet(vertexToNumberOfComponent(vertex))

  /**
   * Get a set of sets, where each set represents a connected component in the graph.
   *
   * @return a set of sets representing connected components in the graph
   */
  def components(): Set[Set[V]] = {
    var connectedComponents = Set[Set[V]]()
    componentNumberToSet.foreach(x => connectedComponents=connectedComponents+(Set.empty++x._2))
    connectedComponents
  }

  /**
   * Get the number of connected components in the graph.
   *
   * @return the number of connected components in the graph
   */
  def numberOfComponents: Int = componentNumberToSet.size

  def areConnected(vertex1:V , vertex2:V):Boolean = vertexToNumberOfComponent(vertex1)==vertexToNumberOfComponent(vertex2)


}

