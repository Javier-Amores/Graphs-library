package graphs
import scala.collection.mutable.Map


trait Graph[V]{
  //def test = println("vengo de Graph")
  def vertices: List[V]
  def edges: Map[V,Set[V]]
  def addVertex(v: V): Unit
  def addVertices(vs: List[V]): Unit
  def addEdge(u: V, v: V): Unit
  def addEdges(vs: List[(V,V)]): Unit
  def removeVertex(v: V): Unit
  def removeVertices(vs: List[V]): Unit
  def removeEdge(u: V, v: V): Unit
  def removeEdges(vs: List[(V,V)]): Unit
  def order: Int //the order is defined as the number of nodes in the graph.
  def size: Int //the size is defined as the number of edges in the graph.
  def degree(v: V): Int //returns the number of edges meeting at v
  def complete(): Unit //makes the graph complete, i.e, each pair of nodes is connected.

}

trait DirectedGraph[V] extends Graph[V]{
  def changeDirection(u: V, v: V)//change direction of edge (U,V)
  def transpose(): Unit //Change direction of all edges
  override def complete(): Unit

}

trait WeightedGraph[V,W] extends Graph[V]{
  def weightedEdges(): Map[V,Set[(V,W)]]
  def addWeight(w: W)


}

trait WeightedDirectedGraph[V,W] extends DirectedGraph[V] with WeightedGraph[V,W]


class MGraph[V] extends Graph[V] {
  private var vertices_ : List[V] = List()
  private var edges_ : Map[V,Set[V]] = Map()

  def vertices: List[V] = vertices_
  def edges: Map[V,Set[V]]  = edges_
  def addVertex(v: V):Unit = if (!(vertices_ contains v)) {vertices_ = v::vertices_
    edges_ += (v -> Set())}

  def addVertices(vs: List[V]):Unit = {
    for (v <- vs) addVertex(v)
  }

  def addEdge(u: V, v: V) = if ((vertices_ contains u) && (vertices_ contains v)) {
    edges_(u) = edges_(u)+v
    edges_(v) = edges_(v)+u
  }

  def addEdges(vs: List[(V,V)]): Unit = {
    for ((u,v) <- vs) addEdge(u,v)
  }

  def removeEdge(u: V, v: V): Unit = if ((vertices_ contains u) && (vertices_ contains v)) {
    edges_(u) = edges_(u) - v
    edges_(v) = edges_(v) - u
  }

  def removeEdges(vs: List[(V,V)]): Unit = {
    for ((u, v) <- vs) removeEdge(u, v)
  }

  def removeVertex(v: V): Unit = if (vertices_ contains v) {
    edges_ -= v
    for ((key,values) <- edges_) {edges_(key) -= v}
    vertices_ = vertices_.filter(_ != v)
  }

  def removeVertices(vs: List[V]): Unit = {
    for (v <- vs) removeVertex(v)
  }

  def order: Int = vertices_.length

  def size: Int = {
    var sum:Int = 0
    for ((key,values) <- edges_) {sum += degree(key)}
    sum/2
  }

  def degree(v: V): Int = if (vertices_ contains v) {edges_(v).size} else -1

  def complete(): Unit = for ((key,values) <- edges_) {
    edges_(key) = vertices_.filter(_ != key).toSet
  }

}



//class Prueba extends WeightedDirectedGraphs[String,Int]

/*
class MapGraph[A](g:Map[A,Set[A]] =  Map[A, Set[A]]()){
  var vertices = g.keySet
  var edges = g.values.toSet

  def addVertex(v: A):Unit = {

  }

}



class AdjacencyMatrixGraph[A](vertices: List[A],edges:List[(Int,Int)]){
  def size: Int = vertices.length
  val adjacencyMatrix :Array[Array[Boolean]] = {
    val matrix = Array.ofDim[Boolean](size, size) //initialize matrix
    //build matrix
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (edges.contains(i,j)) matrix(i)(j) = true else matrix(i)(j) = false
      }
    }
    matrix
  }

  // Print matrix
  def printMatrix():Unit={
    vertices.foreach(v => print(v + ""))
    println()
  for (i <- 0 until  size) {
    print(vertices(i))
    for (j <- 0 until  size) {
      print(" " + adjacencyMatrix(i)(j))
    }
    println()
    }
  }
}

*/

object example extends App{
  //val vertices:List[String]= List("Madrid","Barcelona","Malaga")
  //val edges:List[(Int,Int)] = List((1,2),(0,1))
  //val example = new AdjacencyMatrixGraph[String](vertices, edges)
  //println(example.size)
  //example.printMatrix()

  //val p = new Prueba
  //p.test
  val g = new MGraph[Int]

  println(g.vertices)
  g.addVertex(5)
  println(g.vertices)
  g.addVertex(3)
  println(g.vertices)
  g.addVertices(List(6,7))
  println(g.vertices)
  g.addEdges(List((3,5),(6,7),(7,6)))
  println(g.edges)
}

