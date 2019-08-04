package scalan.util

import scalan.{DFuncAdapter, DFunc}
import debox.{Set => DSet, Buffer => DBuffer, Map => DMap}
import spire.syntax.all.cfor

import scala.reflect.ClassTag

trait NeighbourFunc[@specialized(Int) A, B] {
  def populate(x: A, res: DBuffer[B]): Unit
}
class Neighbours[@specialized(Int) A, B](f: DFunc[A, DBuffer[B]]) extends NeighbourFunc[A, B] {
  override def populate(x: A, res: DBuffer[B]): Unit = {
    val ns = f(x)
    res ++= ns
  }
}

object GraphUtil {

  def depthFirstSetFrom[@specialized(Int) A: ClassTag](starts: DBuffer[A])(neighbours: NeighbourFunc[A, A]): DSet[A] = {
    val visited = DSet.ofSize[A](starts.length)

    def visit(s: A): Unit = {
      if (!(visited(s))) {
        visited += s
        val ns = DBuffer.ofSize[A](16)
        neighbours.populate(s, ns)
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
   * of the graph rooted at the startNodes argument,
   * whose edges are given by the function argument `succ`.
   *
   * @see Tarjan, Robert E.. “Depth-First Search and Linear Graph Algorithms.” SIAM J. Comput. 1 (1972): 146-160.
   */
  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](startNodes: DBuffer[T], succ: DFunc[T, DBuffer[T]]): DBuffer[DBuffer[T]] = {
    val tarjan = new Tarjan[T](succ)

    // top level loop of the algorithm
    cfor(0)(_ < startNodes.length, _ + 1) { i =>
      val node = startNodes(i)
      tarjan.visit(node)
    }

    tarjan.res
  }

  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](start: Array[T])(succ: T => DBuffer[T]): DBuffer[DBuffer[T]] = {
    stronglyConnectedComponents(DBuffer.fromArray(start), new DFuncAdapter(succ))
  }

  val initTarjan = new Tarjan[Int](new DFuncAdapter((i: Int) => DBuffer(i)))
}


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
