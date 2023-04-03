package graph.traversal


trait Structure[T] {
  def add(element: T): Unit

  def add(elements: IterableOnce[T]): Unit

  def remove(): T

  def isEmpty(): Boolean
}

