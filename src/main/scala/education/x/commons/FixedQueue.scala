package education.x.commons

import scala.collection.mutable


class FixedQueue[T](maxSize: Int) extends mutable.Queue[T] {
  override def +=(elem: T): FixedQueue[T] = {
    if (length >= maxSize) dequeue()
    appendElem(elem)
    this
  }
}
