package graph.traversal


/**
 * This trait represents a container that holds elements of type T.
 * @tparam T  the type of the elements held in the container
 */
protected trait Container[T] {
  /**
   * Adds a single element to the container.
   * @param element the element to add
   */
  def add(element: T): Unit

  /**
   * Adds all elements in an iterable to the container.
   * @param elements the iterable containing the elements to add
   */
  def add(elements: IterableOnce[T]): Unit

  /**
   * Removes an element from the container.
   * @return the removed element
   */
  def remove(): T

  /**
   * Checks if the container is empty.
   * @return true if the container is empty, false otherwise
   */
  def isEmpty: Boolean
}

