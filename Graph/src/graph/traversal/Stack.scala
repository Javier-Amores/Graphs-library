package graph.traversal

import scala.collection.mutable

/**
 * This class implements a stack data structure that holds elements of type T.
 *
 * @tparam T the type of the elements held in the stack
 */
protected class Stack[T] extends Container[T] {
  private val stack = mutable.Stack[T]()

  /**
   * Adds a single element to the top of the stack.
   *
   * @param element the element to add
   */
  override def add(element: T): Unit = stack.push(element)

  /**
   * Adds all elements in an iterable to the top of the stack.
   *
   * @param elements the iterable containing the elements to add
   */
  override def add(elements: IterableOnce[T]): Unit = stack.pushAll(elements)

  /**
   * Removes the top element from the stack.
   *
   * @return the removed element
   */
  override def remove(): T = stack.pop()

  /**
   * Checks if the stack is empty.
   *
   * @return true if the stack is empty, false otherwise
   */
  override def isEmpty: Boolean = stack.isEmpty
}
