package scalan.util

import scala.reflect.ClassTag
import spire.syntax.all.cfor
import scalan.DFunc
import debox.{Buffer => DBuffer, Map => DMap}

/** Implementation of the SCC part of Tarjan algorithm. */
final class Tarjan[@specialized(Int) T: ClassTag](private var getNeighbours: DFunc[T, DBuffer[T]]) {
  private var id = 0
  private var stack: DBuffer[T] = DBuffer.empty
  private val mark = DMap.ofSize[T,Int](127)

  val res: DBuffer[DBuffer[T]] = DBuffer.empty

  /** Explore the next graph node. Returns quickly if the node is already marked.
    * @return mark assigned to `node` */
  def visit(node: T): Int = {
    val n = mark.getOrElse(node, -1)
    if (n >= 0) return n

    id += 1

    mark.update(node, id)
    stack += node

    var min: Int = id
    val neighbours = getNeighbours(node)
    cfor(0)(_ < neighbours.length, _ + 1) { i =>
      val child = neighbours(i)
      val m = visit(child)

      if (m < min)
        min = m
    }

    if (min == mark(node)) {
      val scc = DBuffer.empty[T]

      var loop: Boolean = true
      do {
        val element = stack.pop
        scc += element
        mark.update(element, Integer.MAX_VALUE)
        loop = element != node
      } while (loop)

      res += scc
    }
    min
  }
}
