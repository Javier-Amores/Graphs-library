package graph

/**
 * A trait representing a weighted edge.
 *
 * @tparam V the type of the vertices in the weighted edge
 * @tparam W the type of the weight in the weighted edge
 */
trait IsWeightedEdge[+V, +W] extends IsEdge[V] with IsWeighted[W]
