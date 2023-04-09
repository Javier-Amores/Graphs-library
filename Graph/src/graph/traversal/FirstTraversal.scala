package graph.traversal


import graph._

import scala.collection.mutable

/**
 * Provides a generic implementation for a first traversal algorithm over a graph, starting from a given vertex.
 * @param graph the graph to traverse
 * @param startVertex the vertex to start the traversal from
 * @tparam V the type of the vertices in the graph
 */
abstract class FirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends Traversal[V] {
  protected val spanningTree: mutable.Map[V, mutable.Set[V]]
  protected val container: Container[IsEdge[V]]

  /**
   * Traverses the graph starting from the specified start vertex, and returns the resulting spanning tree.
   * @return the resulting spanning tree
   */
  // Generic implementation of traversal
  protected def traverse(): mutable.Map[V, mutable.Set[V]] = {
    if (graph.containsVertex(startVertex)) {
      val visited = mutable.Set[V]()
      val tree = mutable.Map[V, mutable.Set[V]]()
      container.add(Edge(startVertex, startVertex))
      while (!container.isEmpty) {
        val currentEdge = container.remove()
        if (!visited.contains(currentEdge.vertex2)) {
          visited += currentEdge.vertex2
          tree.get(currentEdge.vertex1) match {
            case None => tree(currentEdge.vertex1) = mutable.Set(currentEdge.vertex2)
            case Some(set) => set += currentEdge.vertex2
          }
          val succs: IterableOnce[IsEdge[V]] = graph.incidentsFrom(currentEdge.vertex2).filterNot(edge => visited.contains(edge.vertex2))
          container.add(succs)
        }
      }
      tree

    }
    else {
      throw GraphException(s"Vertex $startVertex not found")
    }
  }


  // Implementation of query methods that are the same for both subclasses.
  override def getSpanningTree: mutable.Map[V, mutable.Set[V]] = spanningTree

  override def isReachable(vertex: V): Boolean = {
    spanningTree.find(x => x._2.contains(vertex)) match {
      case None => false
      case Some(_) => true
    }
  }


  override def pathTo(vertex: V): Option[List[V]] = {
    var path = List[V](vertex)
    var currentVertex:V = vertex
    spanningTree.find(x => x._2.contains(currentVertex)) match{
      case None => return None
      case Some((key,_)) => path =  key +: path
                            currentVertex = key
    }
    while (currentVertex != startVertex) {
      spanningTree.find(x => x._2.contains(currentVertex)) match {
        case Some((key, _)) => path =  key +: path
                                currentVertex = key
      }
    }
    Some(path)
  }
}
