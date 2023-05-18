package graph.shortestPath


/**
 * A trait representing the Dijkstra's shortest path algorithm.
 *
 * @tparam V The type of vertices in the graph
 * @tparam W The type of weights
 * @tparam E The type of edges connecting vertices
 */
trait DijkstraShortestPath[V, W, +E[_]] extends ShortestPath[V, W, E] {

}
