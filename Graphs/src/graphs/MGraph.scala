package graphs
import scala.collection.mutable.Map

class MGraph[V] extends Graph[V] {
  private var graph_ : Map[V,Set[V]] = Map()

  def vertices: Set[V] = graph_.keys.toSet

  def edges: Set[Edge[V]] = {
    var edgeSet: Set[Edge[V]] = Set()
    for ((source, destinationSet) <- graph_) {
      for (destination <- destinationSet) {
        edgeSet = edgeSet + Edge(source, destination)
      }
    }
    edgeSet //each edge is twice (eg, (a,b) ,(b,a))
  }

  def addVertex(vertex: V):Unit = if (!(this.vertices contains vertex)) {graph_ = graph_ + (vertex -> Set())} else {throw  GraphException(s"Vertex $vertex is already in the graph.")}

  def addVertices(vertexSet: Set[V]):Unit = {
    for (vertex <- vertexSet) addVertex(vertex)
  }

  def addEdge(source: V, destination: V):Unit = if ((this.vertices contains source) && (this.vertices contains destination)) {
    graph_(source) = graph_(source)+destination
    graph_(destination) = graph_(destination)+source
  } else {throw  GraphException(s"Vertices $source and/or $destination don't exist.")}

  def addEdge(edge: Edge[V]): Unit = if ((this.vertices contains edge.source) && (this.vertices contains edge.destination)) {
    graph_(edge.source) = graph_(edge.source) + edge.destination
    graph_(edge.destination) = graph_(edge.destination) + edge.source
  } else {throw  GraphException(s"Vertices ${edge.source} and/or ${edge.destination} don't exist.")}

  def addEdges(edgeSet: Set[Edge[V]]): Unit = {
    for (edge <- edgeSet) addEdge(edge)
  }


  def removeEdge(source: V, destination: V): Unit = if ((this.vertices contains source) && (this.vertices contains destination)) {
    graph_(source) = graph_(source) - destination
    graph_(destination) = graph_(destination) - source
  } else {
    throw GraphException(s"Vertices $source and/or $destination don't exist.")
  }

  def removeEdge(edge: Edge[V]): Unit = if ((this.vertices contains edge.source) && (this.vertices contains edge.destination)) {
    graph_(edge.source) = graph_(edge.source) - edge.destination
    graph_(edge.destination) = graph_(edge.destination) - edge.source
  } else {
    throw GraphException(s"Vertices ${edge.source} and/or ${edge.destination} don't exist.")
  }


  def removeEdges(edgeSet: Set[Edge[V]]): Unit = {
    for (edge <- edgeSet) removeEdge(edge)
  }

  def removeVertex(vertex: V): Unit = if (this.vertices contains vertex) {
    graph_ -= vertex
    for ((node,_) <- graph_) {graph_(node) -= vertex}
  }

  def removeVertices(vertexSet: Set[V]): Unit = {
    for (vertex <- vertexSet) removeVertex(vertex)
  }

  def order: Int = graph_.size

  def size: Int = this.edges.size/2

  def degree(vertex: V): Int = if (this.vertices contains vertex) {graph_(vertex).size} else {throw GraphException(s"Vertex $vertex not found")}

  def complete(): Unit = for ((node,_) <- graph_) {
    graph_(node) = this.vertices.filter(_ != node)
  }

  def getAdjacentNodes(vertex: V): Set[V] = graph_(vertex)

}

