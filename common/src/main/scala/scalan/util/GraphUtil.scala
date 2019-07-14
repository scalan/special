package scalan.util

import scala.collection.mutable.{Buffer, ArrayBuffer}
import scalan.{AVHashMap, DFunc, Nullable, DFuncAdapter}
import debox.{Set => DSet, Buffer => DBuffer, Map => DMap}
import spire.syntax.all.cfor
import scala.reflect.ClassTag

trait NeighbourFunc[@specialized(Int) A] {
  def get(x: A, res: DBuffer[A]): Unit
}
case class Neighbours[@specialized(Int) A](f: A => Array[A]) extends NeighbourFunc[A] {
  override def get(x: A, res: DBuffer[A]): Unit = {
    val ns = f(x)
    res ++= ns
  }
}

object GraphUtil {

  def depthFirstSetFrom[@specialized(Int) A: ClassTag](starts: DBuffer[A])(neighbours: NeighbourFunc[A]): DSet[A] = {
    val visited = DSet.ofSize[A](starts.length)

    def visit(s: A): Unit = {
      if (!(visited(s))) {
        visited += s
        val ns = DBuffer.ofSize[A](16)
        neighbours.get(s, ns)
        cfor(0)(_ < ns.length, _ + 1) { i =>
          visit(ns(i))
        }
      }
    }

   cfor(0)(_ < starts.length, _ + 1) { i =>
     visit(starts(i))
   }

   visited
 }

  /**
   * Returns the strongly connected components
   * of the graph rooted at the first argument,
   * whose edges are given by the function argument.
   *
   * The scc are returned in _reverse_ topological order.
   * Tarjan's algorithm (linear).
   */
  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](startNodes: Array[T], succ: DFunc[T, Array[T]]): DBuffer[DBuffer[T]] = {
    val tarjan = new Tarjan[T](succ)

    cfor(0)(_ < startNodes.length, _ + 1) { i =>
      val node = startNodes(i)
      tarjan.visit(node)
    }

    tarjan.res
  }

  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](start: Array[T])(succ: T => Array[T]): DBuffer[DBuffer[T]] = {
    stronglyConnectedComponents(start, new DFuncAdapter(succ))
  }

}


final class Tarjan[@specialized(Int) T: ClassTag](getNeighbours: DFunc[T, Array[T]]) {
  private var id = 0
  private var stack: DBuffer[T] = DBuffer.empty
  private val mark = DMap.ofSize[T,Int](127)

  val res: DBuffer[DBuffer[T]] = DBuffer.empty

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
