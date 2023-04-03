package graph.traversal


import graph._

import scala.collection.mutable

abstract class FirstTraversal[V](graph: Graph[V, IsEdge], startVertex: V) extends Traversal[V] {
  protected val spanningTree: mutable.Map[V, mutable.Set[V]]
  protected val structure: Structure[IsEdge[V]]

  // implementación genérica del recorrido
  protected def traverse(): mutable.Map[V, mutable.Set[V]] = {
    if (graph.containsVertex(startVertex)) {
      val visited = mutable.Set[V]()
      val tree = mutable.Map[V, mutable.Set[V]]()
      structure.add(Edge(startVertex, startVertex))
      while (!structure.isEmpty) {
        val currentEdge = structure.remove()
        if (!visited.contains(currentEdge.vertex2)) {
          visited += currentEdge.vertex2
          tree.get(currentEdge.vertex1) match {
            case None => tree(currentEdge.vertex1) = mutable.Set(currentEdge.vertex2)
            case Some(set) => set += currentEdge.vertex2
          }
          val succs: IterableOnce[IsEdge[V]] = graph.incidentsFrom(currentEdge.vertex2).filterNot(edge => visited.contains(edge.vertex2))
          structure.add(succs)
        }
      }
      tree

    }
    else {
      throw GraphException(s"Vertex $startVertex not found")
    }
  }


  // implementación de los métodos de consulta que son iguales para ambas subclases
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
