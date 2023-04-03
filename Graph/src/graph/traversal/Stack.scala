package graph.traversal

import scala.collection.mutable

class Stack[T] extends Structure[T] {
  private val stack = mutable.Stack[T]()

  override def add(element: T): Unit = stack.push(element)

  override def add(elements: IterableOnce[T]): Unit = stack.pushAll(elements)

  override def remove(): T = stack.pop()

  override def isEmpty(): Boolean = stack.isEmpty
}
