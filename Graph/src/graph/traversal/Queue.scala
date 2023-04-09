package graph.traversal

import scala.collection.mutable

/**
 * This class implements a queue data structure that holds elements of type T.
 *
 * @tparam T the type of the elements held in the queue
 */
protected class Queue[T] extends Container[T] {
  private val queue = mutable.Queue[T]()

  /**
   * Adds a single element to the back of the queue.
   *
   * @param element the element to add
   */
  override def add(element: T): Unit = queue.enqueue(element)

  /**
   * Adds all elements in an iterable to the back of the queue.
   *
   * @param elements the iterable containing the elements to add
   */
  override def add(elements: IterableOnce[T]): Unit = queue.enqueueAll(elements)

  /**
   * Removes the front element from the queue.
   *
   * @return the removed element
   */
  override def remove(): T = queue.dequeue()

  /**
   * Checks if the queue is empty.
   *
   * @return true if the queue is empty, false otherwise
   */
  override def isEmpty: Boolean = queue.isEmpty
}
