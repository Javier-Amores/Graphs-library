package graphs

trait WeightedDirectedGraph[V,W] extends DirectedGraph[V] with WeightedGraph[V,W]
