package graph.traversal

import graph.UndirectedGraph

import scala.collection.{immutable, mutable}


/**
 * An abstract class for computing connected components of an undirected graph.
 *
 * @param graph the graph to compute the connected components of
 * @tparam V the type of the vertices in the graph
 */
abstract class ConnectedComponent[V](graph: UndirectedGraph[V]) {

  private val vertexToNumberOfComponent: mutable.Map[V, Int] = mutable.Map[V, Int]()
  private val componentNumberToSet: mutable.Map[Int, mutable.Set[V]] = mutable.Map[Int, mutable.Set[V]]()

  /**
   * Returns a traversal object starting at the specified vertex.
   *
   * @param starVertex the vertex to start the traversal from
   * @return object representing the traversal starting at the specified vertex
   */
  protected def traversal(starVertex: V): FirstTraversal[V]

  /**
   * Private method that computes the connected components of the graph and populates the maps of the class.
   */
  private def computeMaps(): Unit = {
    var vertices = graph.vertices.iterator
    var counter: Int = 0
    while (vertices.nonEmpty) {
      val vertex = vertices.iterator.next()
      componentNumberToSet(counter) = mutable.Set[V]()
      val traversalOfVertex = traversal(vertex)
      traversalOfVertex.getSpanningTree.foreach { case (successor, _) => vertexToNumberOfComponent(successor) = counter
        componentNumberToSet(counter) += successor
        vertices = vertices.filterNot(node => node == successor)
      }

      counter += 1
    }
  }

  computeMaps()


  /**
   * Get the set of vertices in the same connected component as the specified vertex.
   *
   * @param vertex the vertex to find the connected component of
   * @return a set of vertices in the same connected component
   */
  def componentOf(vertex: V): immutable.Set[V] = Set.empty ++ componentNumberToSet(vertexToNumberOfComponent(vertex))

  /**
   * Get a set of sets, where each set represents a connected component in the graph.
   *
   * @return a set of sets representing connected components in the graph
   */
  def components(): immutable.Set[Set[V]] = {
    var connectedComponents = Set[Set[V]]()
    componentNumberToSet.foreach(x => connectedComponents = connectedComponents + (Set.empty ++ x._2))
    connectedComponents
  }

  /**
   * Get the number of connected components in the graph.
   *
   * @return the number of connected components in the graph
   */
  def numberOfComponents: Int = componentNumberToSet.size

  /**
   * Checks if a pair or nodes are in the same connected component.
   *
   * @param vertex1 The first vertex
   * @param vertex2 The second vertex
   * @return True if the nodes are in the same connected component, false otherwise
   */
  def areConnected(vertex1: V, vertex2: V): Boolean = vertexToNumberOfComponent(vertex1) == vertexToNumberOfComponent(vertex2)


}

