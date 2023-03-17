package graph

/**
 * A trait representing a directed weighted edge.
 *
 * @tparam V the type of the vertices in the directed weighted edge
 * @tparam W the type of the weight in the directed weighted edge
 */
trait IsDirectedWeightedEdge[+V, +W] extends IsDirectedEdge[V] with IsWeighted[W]
