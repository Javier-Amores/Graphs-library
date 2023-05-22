package graph.priorityQueue

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.pow

/**
 * A priority queue implementation with support for indexing. This priority queue is implemented using a binary heap data structure.
 *
 * @param maxN The maximum number of elements the priority queue can hold
 * @param ord  The ordering used to compare elements in the priority queue
 * @tparam A The type of elements stored in the priority queue
 */
case class IndexPriorityQueue[A](maxN: Int)(implicit val ord: Ordering[A]) {

  private var N: Int = 0
  private val pq: Array[Int] = Array.fill(maxN + 1)(0)
  private val qp: Array[Int] = Array.fill(maxN)(-1)
  private val items: mutable.Map[Int, A] = mutable.Map()


  /**
   * Determine if item at index i is greater than item at index j.
   *
   * @param i The index of the first item
   * @param j The index of the second item
   * @return true if the item at index i is greater than the item at index j, false otherwise
   */
  private def more(i: Int, j: Int): Boolean = {
    ord.compare(items(pq(i)), items(pq(j))) > 0
  }


  /**
   * Exchange items at indices i and j in the priority queue.
   *
   * @param i The index of the first item
   * @param j The index of the second item
   */
  private def exchange(i: Int, j: Int): Unit = {
    val t = pq(i)
    pq(i) = pq(j)
    pq(j) = t

    qp(pq(i)) = i
    qp(pq(j)) = j
  }

  /**
   * Restores the heap property by promoting the item at index k to its correct position.
   *
   * @param k The index of the item to be promoted
   */
  @tailrec
  private def swim(k: Int): Unit = {
    if (k > 1 && more(k / 2, k)) {
      exchange(k / 2, k)
      swim(k / 2)
    }
  }

  /**
   * Restores the heap property by demoting the item at index k to its correct position.
   *
   * @param k The index of the item to be demoted
   */
  @tailrec
  private def sink(k: Int): Unit = {
    var j: Int = pow(2, k).toInt
    if (j <= N) {
      if (j < N && more(j, j + 1)) {
        j += 1
      }
      if (!more(k, j)) {

      } else {
        exchange(k, j)
        sink(j)
      }
    }
  }

  /**
   * Enqueues an item into the priority queue.
   *
   * @param k    The index of the item
   * @param item The item to be enqueued
   */
  def enqueue(k: Int, item: A): Unit = {
    N += 1
    qp(k) = N
    pq(N) = k
    items(k) = item
    swim(N)
  }

  /**
   * Updates the item at the given index in the priority queue.
   *
   * @param k    The index of the item
   * @param item The updated item
   */
  def update(k: Int, item: A): Unit = {
    items(k) = item
    swim(qp(k))
    sink(qp(k))
  }

  /**
   * Checks if the priority queue contains an item with the given index.
   *
   * @param k The index to be checked
   * @return true if the priority queue contains the index, false otherwise
   */
  def contains(k: Int): Boolean = {
    qp(k) != -1
  }

  /**
   * Removes the item with the given index from the priority queue.
   *
   * @param k The index of the item to be removed
   */
  def remove(k: Int): Unit = {
    exchange(qp(k), N)
    N -= 1
    swim(qp(k))
    sink(qp(k))
    items.remove(pq(N + 1))
    qp(pq(N + 1)) = -1
  }

  /**
   * Returns an Option containing the item with the highest priority in the priority queue.
   *
   * @return An Option containing the item if the priority queue is not empty, None otherwise
   */
  def head(): Option[A] = {
    if (nonEmpty()) {
      Some(items(pq(1)))
    }
    else {
      None
    }
  }

  /**
   * Returns an Option containing the index of the item with the highest priority in the priority queue.
   *
   * @return An Option containing the index if the priority queue is not empty, None otherwise
   */
  def headIndex(): Option[Int] = {
    if (nonEmpty()) {
      Some(pq(1))
    }
    else {
      None
    }
  }

  /**
   * Dequeues and removes the item with the highest priority from the priority queue.
   *
   * @return The index of the item with the highest priority
   */
  def dequeue(): Int = {
    val indexOfMin = pq(1)
    exchange(1, N)
    N -= 1
    sink(1)
    items.remove(pq(N + 1))
    qp(pq(N + 1)) = -1
    indexOfMin
  }

  /**
   * Checks if the priority queue is empty.
   *
   * @return true if the priority queue is empty, false otherwise
   */
  def isEmpty: Boolean = {
    N == 0
  }

  /**
   * Checks if the priority queue is not empty.
   *
   * @return true if the priority queue is not empty, false otherwise
   */
  def nonEmpty(): Boolean = {
    N != 0
  }

  /**
   * Returns the number of items in the priority queue.
   *
   * @return The number of items in the priority queue
   */
  def size(): Int = N
}
