package graph.traversal


import graph._

import scala.collection.mutable

/**
 * Provides a generic implementation for a first traversal algorithm over a graph, starting from a given vertex.
 *
 * @param graph       the graph to traverse
 * @param startVertex the vertex to start the traversal from
 * @tparam V the type of the vertices in the graph
 */
abstract class FirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends Traversal[V] {
  protected val spanningTree: mutable.Map[V, V]
  protected val container: Container[IsEdge[V]]

  /**
   * Traverses the graph starting from the specified start vertex, and returns the resulting spanning tree.
   *
   * @return the resulting spanning tree
   */
  // Generic implementation of traversal
  protected def traverse(): mutable.Map[V, V] = {
    if (graph.containsVertex(startVertex)) {
      val visited = mutable.Set[V]()
      val tree = mutable.Map[V, V]()
      container.add(Edge(startVertex, startVertex))
      while (!container.isEmpty) {
        val currentEdge = container.remove()
        val currentSuccessor = currentEdge.vertex2
        if (!visited.contains(currentSuccessor)) {
          visited += currentSuccessor
          tree(currentSuccessor) = currentEdge.vertex1
          val nextSuccessors: IterableOnce[IsEdge[V]] = graph.incidentsFrom(currentSuccessor).filterNot(edge => visited.contains(edge.vertex2))
          container.add(nextSuccessors)
        }
      }
      tree

    }
    else {
      throw GraphException(s"Vertex $startVertex not found")
    }
  }


  // Implementation of query methods that are the same for both subclasses.
  override def getSpanningTree: mutable.Map[V, V] = spanningTree

  override def isReachable(vertex: V): Boolean = {
    spanningTree.get(vertex) match {
      case None => false
      case Some(_) => true
    }
  }


  override def pathTo(vertex: V): Option[List[V]] = pathTo(vertex, List[V]())

  protected def pathTo(vertex: V, path: List[V]): Option[List[V]] = {
    spanningTree.get(vertex) match {
      case None => None
      case Some(parentVertex) => if (parentVertex == vertex) {
        Some(parentVertex +: path)
      }
      else {
        pathTo(parentVertex, vertex +: path)
      }
    }
  }


}