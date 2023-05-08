package graph

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.pow

protected case class IndexPriorityQueue[A](maxN: Int)(implicit val ord: Ordering[A]) {

  private var N: Int = 0
  private val pq: Array[Int] = Array.fill(maxN + 1)(0)
  private val qp: Array[Int] = Array.fill(maxN)(-1)
  private val items: mutable.Map[Int, A] = mutable.Map()


  private def more(i: Int, j: Int): Boolean = {
    ord.compare(items(pq(i)), items(pq(j))) > 0
  }


  private def exchange(i: Int, j: Int): Unit = {
    val t = pq(i)
    pq(i) = pq(j)
    pq(j) = t

    qp(pq(i)) = i
    qp(pq(j)) = j
  }

  @tailrec
  private def swim(k: Int): Unit = {
    if (k > 1 && more(k / 2, k)) {
      exchange(k / 2, k)
      swim(k / 2)
    }
  }

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

  def enqueue(k: Int, item: A): Unit = {
    N += 1
    qp(k) = N
    pq(N) = k
    items(k) = item
    swim(N)
  }

  def update(k: Int, item: A): Unit = {
    items(k) = item
    swim(qp(k))
    sink(qp(k))
  }

  def contains(k: Int): Boolean = {
    qp(k) != -1
  }

  def remove(k: Int): Unit = {
    exchange(qp(k), N)
    N -= 1
    swim(qp(k))
    sink(qp(k))
    items.remove(pq(N + 1))
    qp(pq(N + 1)) = -1
  }

  def head(): Option[A] = {
    if (nonEmpty()) {
      Some(items(pq(1)))
    }
    else {
      None
    }
  }

  def headIndex(): Option[Int] = {
    if (nonEmpty()) {
      Some(pq(1))
    }
    else {
      None
    }
  }

  def dequeue(): Int = {
    val indexOfMin = pq(1)
    exchange(1, N)
    N -= 1
    sink(1)
    items.remove(pq(N + 1))
    qp(pq(N + 1)) = -1
    indexOfMin
  }

  def isEmpty: Boolean = {
    N == 0
  }

  def nonEmpty(): Boolean = {
    N != 0
  }

  def size(): Int = N
}
