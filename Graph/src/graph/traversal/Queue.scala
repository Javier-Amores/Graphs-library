package graph.traversal

import scala.collection.mutable

class Queue[T] extends Structure[T] {
  private val queue = mutable.Queue[T]()

  override def add(element: T): Unit = queue.enqueue(element)

  override def add(elements: IterableOnce[T]): Unit = queue.enqueueAll(elements)

  override def remove(): T = queue.dequeue()

  override def isEmpty(): Boolean = queue.isEmpty
}
